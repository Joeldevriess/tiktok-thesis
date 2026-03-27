# ============================================================
# 03_build_variables.R
# Bouwt de vier onderzoeksvariabelen per deelnemer per week:
#   1. Discovery Ratio   – aandeel video's van nooit-eerder-geziene creators
#   2. Session Duration  – gemiddelde sessieduur in minuten
#   3. Like-Rate         – likes / bekeken video's
#   4. Search Intensity  – aantal zoekopdrachten
#
# Vereist: resultaat van 01_inspect_json.R (watch_raw_pXX in environment)
#          creator_map_final_p05.csv
# ============================================================

library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# ── Instellingen ────────────────────────────────────────────
SESSION_GAP_MINS <- 30   # minuten stilte → nieuwe sessie
WEEK_START       <- 1    # 1 = maandag (ISO)

# ── Helper: samenvattende statistieken per deelnemer ────────
summarise_participant <- function(
    video_browse,
    likes,
    favorites,
    creator_map,
    participant_id,
    session_gap_mins = SESSION_GAP_MINS
) {
  cat(rep("=", 55), "\n", sep = "")
  cat("  SAMENVATTENDE STATISTIEKEN –", toupper(participant_id), "\n")
  cat(rep("=", 55), "\n", sep = "")
  
  # Creator lookup
  lookup <- creator_map %>%
    filter(!is.na(creator_username)) %>%
    select(url = input_url, creator_username)
  
  browse <- video_browse %>%
    rename(url = `Video watched`, ts_raw = `Time and Date`) %>%
    mutate(datetime = ymd_hms(ts_raw, tz = "UTC")) %>%
    left_join(lookup, by = "url") %>%
    filter(!is.na(creator_username)) %>%
    arrange(datetime)
  
  # ── 1. Aantal unieke creators ──────────────────────────
  n_unique <- n_distinct(browse$creator_username)
  cat(sprintf("\n Aantal unieke creators gezien: %d\n", n_unique))
  
  # ── 2. Top 10 creators ────────────────────────────────
  top10 <- browse %>%
    count(creator_username, name = "n_videos") %>%
    arrange(desc(n_videos)) %>%
    slice_head(n = 10)
  
  cat("\n Top 10 meest bekeken creators:\n")
  for (i in seq_len(nrow(top10))) {
    cat(sprintf("   %2d. %-38s %d video's\n",
                i, top10$creator_username[i], top10$n_videos[i]))
  }
  
  # ── 3. Top 5 langste sessies ──────────────────────────
  b <- video_browse %>%
    rename(ts_raw = `Time and Date`) %>%
    mutate(datetime = ymd_hms(ts_raw, tz = "UTC")) %>%
    arrange(datetime) %>%
    mutate(
      gap_mins   = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
      new_sess   = is.na(gap_mins) | gap_mins > session_gap_mins,
      session_id = cumsum(new_sess)
    )
  
  all_sess <- b %>%
    group_by(session_id) %>%
    summarise(
      session_start    = min(datetime),
      session_dur_mins = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
      .groups = "drop"
    ) %>%
    arrange(desc(session_dur_mins))
  
  top_sess <- slice_head(all_sess, n = 5)
  
  cat(sprintf("\n Totaal aantal sessies: %d\n", nrow(all_sess)))
  
  cat("\n Top 5 langste sessies:\n")
  for (i in seq_len(nrow(top_sess))) {
    cat(sprintf("   %d. %s  →  %.1f min\n",
                i,
                format(top_sess$session_start[i], "%Y-%m-%d %H:%M"),
                top_sess$session_dur_mins[i]))
  }
  
  # ── 4. Favoriete video's ──────────────────────────────
  cat(sprintf("\n Favoriete video's opgeslagen: %d\n", nrow(favorites)))
  if (nrow(favorites) > 0) {
    fav <- favorites %>%
      rename(url = Video, ts_raw = Date) %>%
      mutate(datetime = ymd_hms(ts_raw, tz = "UTC")) %>%
      left_join(lookup, by = "url")
    for (i in seq_len(nrow(fav))) {
      creator <- ifelse(is.na(fav$creator_username[i]), "onbekend", fav$creator_username[i])
      cat(sprintf("   • %s  |  Creator: %s\n",
                  format(fav$datetime[i], "%Y-%m-%d"), creator))
      cat(sprintf("     %s\n", substr(fav$url[i], 1, 70)))
    }
  }
  
  cat(rep("=", 55), "\n\n", sep = "")
}

# ── Helper: verwerk één deelnemer ───────────────────────────
build_panel_for_participant <- function(
    video_browse,   # data.frame: kolommen "Video watched", "Time and Date"
    likes,          # data.frame: kolommen "Video", "Date"
    searches,       # data.frame: kolommen "Search Term", "Date"
    creator_map,    # data.frame: kolommen input_url, creator_username
    participant_id  # string, bijv. "p05"
) {
  
  # ── 1. Browse history opschonen ──────────────────────────
  browse <- video_browse %>%
    rename(url = `Video watched`, ts_raw = `Time and Date`) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    left_join(
      creator_map %>%
        filter(!is.na(creator_username)) %>%
        select(url = input_url, creator_username),
      by = "url"
    ) %>%
    # Behoud alleen video's waarvoor we een creator kennen
    filter(!is.na(creator_username)) %>%
    arrange(datetime)
  
  # ── 2. DISCOVERY RATIO ───────────────────────────────────
  # Een creator is "nieuw" in week W als hun allereerste verschijning
  # in de kijkgeschiedenis binnen week W valt (nooit eerder gezien).
  
  creator_first_week <- browse %>%
    group_by(creator_username) %>%
    summarise(first_week = min(week), .groups = "drop")
  
  browse <- browse %>%
    left_join(creator_first_week, by = "creator_username") %>%
    mutate(is_new_creator = (week == first_week))
  
  discovery_ratio_weekly <- browse %>%
    group_by(week) %>%
    summarise(
      n_videos_mapped      = n(),
      n_new_creator_videos = sum(is_new_creator),
      discovery_ratio      = n_new_creator_videos / n_videos_mapped,
      .groups = "drop"
    )
  
  # ── 3. SESSION DURATION ──────────────────────────────────
  # Sessie: opeenvolgende video's met < SESSION_GAP_MINS tussenpauze.
  # Sessieduur = tijd eerste t/m laatste video in de sessie.
  # Week van sessie = de week waarin de sessie begint.
  
  sessions <- browse %>%
    arrange(datetime) %>%
    mutate(
      gap_mins    = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
      new_session = is.na(gap_mins) | gap_mins > SESSION_GAP_MINS,
      session_id  = cumsum(new_session)
    )
  
  session_stats <- sessions %>%
    group_by(session_id) %>%
    summarise(
      session_start    = min(datetime),
      session_end      = max(datetime),
      session_dur_mins = as.numeric(difftime(session_end, session_start, units = "mins")),
      week             = floor_date(min(datetime), "week", week_start = WEEK_START),
      .groups = "drop"
    )
  
  session_duration_weekly <- session_stats %>%
    group_by(week) %>%
    summarise(
      n_sessions           = n(),
      avg_session_dur_mins = mean(session_dur_mins),
      .groups = "drop"
    )
  
  # ── 4. LIKE-RATE ─────────────────────────────────────────
  # Like-Rate = aantal likes in week W / aantal bekeken video's in week W
  # (gebaseerd op de volledige browse history, niet alleen gemapte video's)
  
  n_videos_per_week <- video_browse %>%
    rename(ts_raw = `Time and Date`) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(n_videos_total = n(), .groups = "drop")
  
  likes_clean <- likes %>%
    rename(ts_raw = Date) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    )
  
  likes_per_week <- likes_clean %>%
    group_by(week) %>%
    summarise(n_likes = n(), .groups = "drop")
  
  like_rate_weekly <- n_videos_per_week %>%
    left_join(likes_per_week, by = "week") %>%
    mutate(
      n_likes   = replace_na(n_likes, 0),
      like_rate = n_likes / n_videos_total
    )
  
  # ── 5. SEARCH INTENSITY ──────────────────────────────────
  # Search Intensity = aantal zoekopdrachten in week W
  
  search_intensity_weekly <- searches %>%
    rename(ts_raw = Date) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(search_intensity = n(), .groups = "drop")
  
  # ── 6. Samenvoegen tot weekpanel ────────────────────────
  # Basis = alle weken met browse-activiteit (inclusief niet-gemapte video's)
  panel <- n_videos_per_week %>%
    left_join(discovery_ratio_weekly,  by = "week") %>%
    left_join(session_duration_weekly, by = "week") %>%
    left_join(
      like_rate_weekly %>% select(week, n_likes, like_rate),
      by = "week"
    ) %>%
    left_join(search_intensity_weekly, by = "week") %>%
    mutate(
      # Weken zonder zoekopdrachten → 0
      search_intensity = replace_na(search_intensity, 0),
      n_likes          = replace_na(n_likes, 0),
      like_rate        = replace_na(like_rate, 0),
      participant      = participant_id
    ) %>%
    arrange(week) %>%
    select(
      participant, week,
      n_videos_total, n_videos_mapped,
      n_new_creator_videos, discovery_ratio,
      n_sessions, avg_session_dur_mins,
      n_likes, like_rate,
      search_intensity
    )
  
  return(panel)
}

# ============================================================
# Uitvoering per deelnemer
# ============================================================

# ── P05 ─────────────────────────────────────────────────────
creator_map_p05 <- read_csv("data/unique_urls/creator_map_final_p05.csv")

panel_p05 <- build_panel_for_participant(
  video_browse   = watch_raw_p05$tiktok_video_browsing_history[[1]],
  likes          = watch_raw_p05$tiktok_like_list[[3]],
  searches       = watch_raw_p05$tiktok_searches[[4]],
  creator_map    = creator_map_p05,
  participant_id = "p05"
)

# ── Zodra andere creator maps beschikbaar zijn, herhaal voor P01/P02/P04/P06 ──
# Voorbeeld (niet uitvoeren totdat creator maps gereed zijn):
#
# creator_map_p01 <- read_csv("data/creator_maps/creator_map_final_p01.csv")
# panel_p01 <- build_panel_for_participant(
#   video_browse   = watch_raw_p01$tiktok_video_browsing_history[[1]],
#   likes          = watch_raw_p01$tiktok_like_list[[3]],
#   searches       = watch_raw_p01$tiktok_searches[[4]],
#   creator_map    = creator_map_p01,
#   participant_id = "p01"
# )

# ============================================================
# Combineer alle panels tot één dataset (uitbreidbaar)
# ============================================================
panel_all <- bind_rows(
  panel_p05
  # panel_p01, panel_p02, panel_p04, panel_p06
)

# ── Samenvattende statistieken per deelnemer ─────────────────
summarise_participant(
  video_browse   = watch_raw_p05$tiktok_video_browsing_history[[1]],
  likes          = watch_raw_p05$tiktok_like_list[[3]],
  favorites      = watch_raw_p05$tiktok_favorite_videos[[2]],
  creator_map    = creator_map_p05,
  participant_id = "p05"
)

# ── Sla op ──────────────────────────────────────────────────
write_csv(panel_all, "gen/analysis/weekly_panel_all.csv")

# ── Snelle check ────────────────────────────────────────────
cat("\n=== Panel samenvatting ===\n")
print(summary(panel_p05 %>% select(discovery_ratio, avg_session_dur_mins,
                                   like_rate, search_intensity)))

cat("\n=== Eerste 10 rijen P05 ===\n")
print(head(panel_p05, 10))

cat("\n=== Aantal weken per deelnemer ===\n")
print(panel_all %>% count(participant))