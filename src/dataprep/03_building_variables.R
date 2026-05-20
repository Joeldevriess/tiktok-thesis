library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

SESSION_GAP_MINS <- 30
WEEK_START       <- 1

alle_deelnemers <- c(
  "p01", "p02", "p03", "p04", "p05", "p06", "p07",
  "p08", "p09", "p10", "p11", "p13", "p14", "p15",
  "p16", "p17", "p18", "p19", "p22", "p23"
)

build_panel_voor_deelnemer <- function(
    video_browse,    
    likes,           
    searches,        
    creator_map,     
    deelnemer_id     
) {
  
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
    filter(!is.na(creator_username)) %>%
    arrange(datetime)
  
  eerste_week_per_creator <- browse %>%
    group_by(creator_username) %>%
    summarise(eerste_week = min(week), .groups = "drop")
  
  browse <- browse %>%
    left_join(eerste_week_per_creator, by = "creator_username") %>%
    mutate(is_nieuwe_creator = (week == eerste_week))
  
  discovery_per_week <- browse %>%
    group_by(week) %>%
    summarise(
      n_videos_gemapped    = n(),
      n_videos_nieuwe_crea = sum(is_nieuwe_creator),
      discovery_ratio      = n_videos_nieuwe_crea / n_videos_gemapped,
      .groups = "drop"
    )
  
  sessies <- browse %>%
    arrange(datetime) %>%
    mutate(
      pauze_mins    = as.numeric(difftime(datetime, lag(datetime), units = "mins")),
      nieuwe_sessie = is.na(pauze_mins) | pauze_mins > SESSION_GAP_MINS,
      sessie_id     = cumsum(nieuwe_sessie)
    )
  
  sessie_stats <- sessies %>%
    group_by(sessie_id) %>%
    summarise(
      sessie_start    = min(datetime),
      sessie_dur_mins = as.numeric(difftime(max(datetime), min(datetime), units = "mins")),
      week            = floor_date(min(datetime), "week", week_start = WEEK_START),
      .groups = "drop"
    )
  
  sessie_duur_per_week <- sessie_stats %>%
    group_by(week) %>%
    summarise(
      n_sessies           = n(),
      gem_sessieduur_mins = mean(sessie_dur_mins),
      .groups = "drop"
    )
  
  videos_per_week <- video_browse %>%
    rename(ts_raw = `Time and Date`) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(n_videos_totaal = n(), .groups = "drop")
  
  likes_per_week <- likes %>%
    rename(ts_raw = Date) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(n_likes = n(), .groups = "drop")
  
  like_rate_per_week <- videos_per_week %>%
    left_join(likes_per_week, by = "week") %>%
    mutate(
      n_likes   = replace_na(n_likes, 0),   # weken zonder likes → 0
      like_rate = n_likes / n_videos_totaal
    )
  
  zoek_per_week <- searches %>%
    rename(ts_raw = Date) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(zoek_intensiteit = n(), .groups = "drop")
  
  panel <- videos_per_week %>%
    left_join(discovery_per_week,   by = "week") %>%
    left_join(sessie_duur_per_week, by = "week") %>%
    left_join(
      like_rate_per_week %>% select(week, n_likes, like_rate),
      by = "week"
    ) %>%
    left_join(zoek_per_week, by = "week") %>%
    mutate(
      zoek_intensiteit = replace_na(zoek_intensiteit, 0),
      n_likes          = replace_na(n_likes, 0),
      like_rate        = replace_na(like_rate, 0),
      like_rate        = ifelse(like_rate > 1, NA, like_rate),
      deelnemer        = deelnemer_id
    ) %>%
    arrange(week) %>%
    select(
      deelnemer, week,
      n_videos_totaal, n_videos_gemapped,
      n_videos_nieuwe_crea, discovery_ratio,
      n_sessies, gem_sessieduur_mins,
      n_likes, like_rate,
      zoek_intensiteit
    )
  
  return(panel)
}

alle_panels <- list()  

for (pid in alle_deelnemers) {
  naam_browse <- paste0("video_browse_", pid)
  naam_likes  <- paste0("likes_", pid)
  naam_zoek   <- paste0("searches_", pid)
  naam_map    <- paste0("data/unique_urls/creator_map_final_", pid, ".csv")
  
  panel <- build_panel_voor_deelnemer(
    video_browse = video_browse,
    likes        = likes,
    searches     = searches,
    creator_map  = creator_map,
    deelnemer_id = pid
  )
  
  alle_panels[[pid]] <- panel
}

panel_all <- bind_rows(alle_panels)

cat("\n══════════════════════════════════════════════════════\n")
cat("  Gecombineerd panel:", nrow(panel_all), "rijen\n")
cat("  Aantal deelnemers: ", n_distinct(panel_all$deelnemer), "\n")
cat("══════════════════════════════════════════════════════\n")

MIN_WEKEN         <- 10
MIN_VIDEOS_PER_WEEK <- 500

deelnemer_stats <- panel_all %>%
  group_by(deelnemer) %>%
  summarise(
    n_weken             = n(),
    gem_videos_per_week = mean(n_videos_totaal),
    .groups = "drop"
  )

deelnemers_estimation <- deelnemer_stats %>%
  filter(
    n_weken             >= MIN_WEKEN,
    gem_videos_per_week >= MIN_VIDEOS_PER_WEEK
  ) %>%
  pull(deelnemer)

panel_estimation <- panel_all %>%
  filter(deelnemer %in% deelnemers_estimation)

cat("\n══ Estimation sample ════════════════════════════════\n")
cat("  Uitgesloten deelnemers:\n")
uitgesloten <- deelnemer_stats %>%
  filter(!deelnemer %in% deelnemers_estimation) %>%
  left_join(
    deelnemer_stats %>% select(deelnemer, n_weken, gem_videos_per_week),
    by = c("deelnemer", "n_weken", "gem_videos_per_week")
  )
for (i in seq_len(nrow(uitgesloten))) {
  cat(sprintf("    %s: %d weken, %.1f videos/week\n",
              uitgesloten$deelnemer[i],
              uitgesloten$n_weken[i],
              uitgesloten$gem_videos_per_week[i]))
}
cat(sprintf("  Overblijvend: %d deelnemers, %d observaties\n",
            n_distinct(panel_estimation$deelnemer),
            nrow(panel_estimation)))
cat("══════════════════════════════════════════════════════\n")

write_csv(panel_all,        "gen/analysis/weekly_panel_all.csv")
write_csv(panel_estimation, "gen/analysis/weekly_panel_estimation.csv")

stats <- panel_estimation %>%
  select(discovery_ratio, gem_sessieduur_mins, like_rate, zoek_intensiteit) %>%
  summarise(across(
    everything(),
    list(
      n    = ~sum(!is.na(.)),
      mean = ~round(mean(., na.rm = TRUE), 3),
      sd   = ~round(sd(.,   na.rm = TRUE), 3),
      min  = ~round(min(.,  na.rm = TRUE), 3),
      max  = ~round(max(.,  na.rm = TRUE), 3),
      n_NA = ~sum(is.na(.))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("variabele", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

print(stats, n = Inf)

cat("\n══ Weken per deelnemer (estimation sample) ══════════\n")
print(panel_estimation %>% count(deelnemer, name = "n_weken"))