# ============================================================
# 03_build_variables.R  —  bijgewerkte versie voor alle deelnemers
#
# Wat doet dit script?
#   Voor elke deelnemer berekenen we vier wekelijkse variabelen:
#     1. Discovery Ratio   – aandeel video's van creators die deze week
#                            voor het EERST gezien worden
#     2. Session Duration  – gemiddelde sessieduur in minuten per week
#     3. Like-Rate         – likes / totaal bekeken video's per week
#     4. Search Intensity  – aantal zoekopdrachten per week
#
# Vereisten:
#   - Script 01 is al gedraaid → video_browse_pXX, likes_pXX en
#     searches_pXX staan al in het geheugen, we gebruiken die direct
#   - Creator map CSV-bestanden staan in: data/unique_urls/
# ============================================================

library(dplyr)
library(lubridate)
library(readr)
library(tidyr)

# ============================================================
# STAP 1: Instellingen
# ============================================================
# SESSION_GAP_MINS = 30: een pauze van >30 minuten = nieuwe sessie.
# Dit is een gangbare grens in onderzoek naar online kijkgedrag.
# WEEK_START = 1: weken beginnen op maandag (ISO-standaard).

SESSION_GAP_MINS <- 30
WEEK_START       <- 1

# ============================================================
# STAP 2: Lijst van alle deelnemers
# ============================================================
# p12 en p20 hebben geen video browse data (zie script 01),
# die laten we weg.

alle_deelnemers <- c(
  "p01", "p02", "p03", "p04", "p05", "p06", "p07",
  "p08", "p09", "p10", "p11", "p13", "p14", "p15",
  "p16", "p17", "p18", "p19", "p22", "p23"
)

# ============================================================
# STAP 3: Kernfunctie — bouw het weekpanel voor één deelnemer
# ============================================================
# Door de logica in één functie te zetten, hoeven we de stappen
# maar één keer te schrijven en roepen we de functie gewoon 14x aan.

build_panel_voor_deelnemer <- function(
    video_browse,    # data frame met alle bekeken video's + tijdstip
    likes,           # data frame met gelikte video's + tijdstip
    searches,        # data frame met zoekopdrachten + tijdstip
    creator_map,     # data frame: input_url → creator_username
    deelnemer_id     # string, bijv. "p05"
) {
  
  # ── 3a. Kijkgeschiedenis koppelen aan creators ─────────────
  # Waarom?
  #   We hebben de creator nodig om te weten of iemand "nieuw" is.
  #   Video's zonder gekende creator gooien we weg voor de
  #   Discovery Ratio — zonder creator kunnen we niet bepalen of
  #   het een nieuwe of bekende creator is.
  #   Voor Like-Rate en Search Intensity gebruiken we ALLE video's
  #   (ook zonder creator), zie stap 3c en 3d.
  
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
  
  # ── 3b. Discovery Ratio ────────────────────────────────────
  # Definitie:
  #   Een creator is "nieuw" in week W als dit de allereerste week
  #   is dat de deelnemer een video van die creator heeft gezien.
  #   Discovery Ratio = video's van nieuwe creators / totaal gemapte video's
  #
  # Voorbeeld: als iemand in week 5 voor het eerst een video ziet van
  # creator X, dan telt die video mee als "nieuw" in week 5 — ook als
  # hij daarna nog 10 video's van X ziet in diezelfde week.
  
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
  
  # ── 3c. Session Duration ───────────────────────────────────
  # Definitie:
  #   Sessie = aaneengesloten kijkreeks waarbij de pauze tussen
  #   twee opeenvolgende video's korter is dan SESSION_GAP_MINS.
  #   Sessieduur = tijd van eerste t/m laatste video in de sessie.
  #   We berekenen het gemiddelde per week.
  #
  # cumsum(nieuwe_sessie) werkt als een oplopende teller:
  #   elke keer dat er een nieuwe sessie begint, gaat de sessie_id
  #   met 1 omhoog. Zo krijgt elke video een sessie-label.
  
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
  
  # ── 3d. Like-Rate ──────────────────────────────────────────
  # Definitie:
  #   Like-Rate = likes in week W / totaal bekeken video's in week W
  #
  # Waarom de VOLLEDIGE browse history als noemer?
  #   Een deelnemer kan een video liken die niet in de creator map zit
  #   (bijv. omdat de URL niet gekropen kon worden). Als we alleen
  #   gemapte video's tellen, overschatten we de like-rate.
  
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
  
  # ── 3e. Search Intensity ───────────────────────────────────
  # Definitie:
  #   Aantal zoekopdrachten per week. Weken zonder zoekopdrachten
  #   krijgen de waarde 0 (niet NA).
  
  zoek_per_week <- searches %>%
    rename(ts_raw = Date) %>%
    mutate(
      datetime = ymd_hms(ts_raw, tz = "UTC"),
      week     = floor_date(datetime, "week", week_start = WEEK_START)
    ) %>%
    group_by(week) %>%
    summarise(zoek_intensiteit = n(), .groups = "drop")
  
  # ── 3f. Alles samenvoegen tot één weekpanel ────────────────
  # Waarom beginnen bij videos_per_week?
  #   Dit data frame bevat ALLE weken met kijkactiviteit als basis.
  #   Via left_join voegen we de andere variabelen toe.
  #   Zo vallen weken niet weg als er toevallig geen likes of
  #   zoekopdrachten waren in die week.
  
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

# ============================================================
# STAP 4: Loop over alle deelnemers
# ============================================================
# Waarom get() gebruiken?
#   Na script 01 staan de tabellen al klaar als losse variabelen:
#   video_browse_p05, likes_p05, searches_p05, enzovoort.
#   Met get("video_browse_p05") kunnen we die naam dynamisch
#   opbouwen in de loop — zo hoeven we geen code te herhalen.
#
# Waarom een exists()-check vooraf?
#   Als script 01 niet voor een bepaalde deelnemer gedraaid is,
#   krijg je een duidelijke waarschuwing in plaats van een
#   cryptische fout halverwege de loop.

alle_panels <- list()  # verzamellijst voor de panels van alle deelnemers

for (pid in alle_deelnemers) {
  
  cat("\n── Verwerken:", pid, "────────────────────────────────\n")
  
  # Bouw de variabelenamen op zoals script 01 ze aanmaakt
  naam_browse <- paste0("video_browse_", pid)
  naam_likes  <- paste0("likes_", pid)
  naam_zoek   <- paste0("searches_", pid)
  naam_map    <- paste0("data/unique_urls/creator_map_final_", pid, ".csv")
  
  # ── Check 1: staan de variabelen in het geheugen? ─────────
  if (!exists(naam_browse) || !exists(naam_likes) || !exists(naam_zoek)) {
    cat("  ⚠ Overgeslagen: één of meer variabelen niet gevonden in geheugen.\n")
    cat("    Nodig:", naam_browse, "/", naam_likes, "/", naam_zoek, "\n")
    cat("    Zorg dat script 01 volledig gedraaid is voor", pid, "\n")
    next
  }
  
  # ── Check 2: bestaat de creator map? ──────────────────────
  if (!file.exists(naam_map)) {
    cat("  ⚠ Overgeslagen: creator map niet gevonden op:", naam_map, "\n")
    next
  }
  
  # ── Haal de variabelen op en laad de creator map ──────────
  video_browse <- get(naam_browse)
  likes        <- get(naam_likes)
  searches     <- get(naam_zoek)
  creator_map  <- read_csv(naam_map, show_col_types = FALSE)
  
  # ── Check 3: zijn de variabelen ook écht gevuld? ──────────
  # exists() controleert alleen of de naam bestaat, niet of er data in zit.
  # Een NULL-waarde passeert exists() maar crasht later in rename().
  
  if (is.null(video_browse) || nrow(video_browse) == 0) {
    cat("  ⚠ Overgeslagen:", naam_browse, "is NULL of leeg.\n")
    next
  }
  if (is.null(likes)) {
    cat("  ⚠ Waarschuwing: likes leeg voor", pid, "— wordt als 0 behandeld.\n")
    likes <- data.frame(Date = character(0))  # leeg maar geldig data frame
  }
  if (is.null(searches)) {
    cat("  ⚠ Waarschuwing: searches leeg voor", pid, "— wordt als 0 behandeld.\n")
    searches <- data.frame(Date = character(0))
  }
  
  # ── Bouw het weekpanel voor deze deelnemer ─────────────────
  panel <- build_panel_voor_deelnemer(
    video_browse = video_browse,
    likes        = likes,
    searches     = searches,
    creator_map  = creator_map,
    deelnemer_id = pid
  )
  
  alle_panels[[pid]] <- panel
  cat("  ✓ Klaar:", nrow(panel), "weken\n")
}

# ============================================================
# STAP 5: Combineer tot één dataset
# ============================================================
# bind_rows() stapelt alle deelnemer-panels verticaal op elkaar.
# Het resultaat is een "long format" panel: elke rij is één week
# voor één deelnemer. Dit is de standaard structuur voor
# panelregressies (deelnemer × week als primary key).

panel_all <- bind_rows(alle_panels)

cat("\n══════════════════════════════════════════════════════\n")
cat("  Gecombineerd panel:", nrow(panel_all), "rijen\n")
cat("  Aantal deelnemers: ", n_distinct(panel_all$deelnemer), "\n")
cat("══════════════════════════════════════════════════════\n")

# ============================================================
# STAP 6: Opslaan
# ============================================================
# Dit is het bestand dat je gebruikt voor alle verdere analyses.
# CSV is geschikt omdat het platformonafhankelijk is en makkelijk
# te openen in R, Excel of Python.

write_csv(panel_all, "gen/analysis/weekly_panel_all.csv")
cat("\n✓ Opgeslagen: gen/analysis/weekly_panel_all.csv\n")

# ============================================================
# STAP 7: Beschrijvende statistieken
# ============================================================
# n     = aantal niet-ontbrekende waarden
# mean  = gemiddelde
# sd    = standaarddeviatie
# min   = laagste waarde
# max   = hoogste waarde
# n_NA  = aantal ontbrekende waarden (zou 0 moeten zijn)

cat("\n══ Beschrijvende statistieken (alle deelnemers) ══════\n")

stats <- panel_all %>%
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

cat("\n══ Weken per deelnemer ═══════════════════════════════\n")
print(panel_all %>% count(deelnemer, name = "n_weken"))