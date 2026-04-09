# ============================================================
# 04_deleted_videos_audit.R
#
# Vergelijkt creator_map_final_pXX.csv met unique_video_urls_pXX.csv
# om te bepalen hoeveel video's verwijderd zijn (of anderszins
# niet opgelost konden worden), waardoor creator-data ontbreekt.
#
# Output:
#   - Samenvatting per deelnemer (console)
#   - Gecombineerde auditdata: gen/temp/deleted_videos_audit.csv
#
# Vereist bestanden:
#   gen/temp/unique_video_urls_pXX.csv   (output van 02_unique_urls.R)
#   data/unique_urls/creator_map_final_pXX.csv
# ============================================================

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# ── Deelnemers ───────────────────────────────────────────────
participants <- c(
  "p01", "p02", "p03", "p04", "p05", "p06", "p07",
  "p08", "p09", "p10", "p11", "p13", "p14", "p15",
  "p16", "p17", "p18", "p19", "p22", "p23"
)

# ── Hulpfunctie: categoriseer reden van ontbrekende creator ──
categorise_failure <- function(status_code, error, creator_username) {
  case_when(
    !is.na(creator_username)               ~ "OK",
    status_code == 404                     ~ "Verwijderd (404)",
    status_code == 403                     ~ "Privé / geblokkeerd (403)",
    status_code %in% c(301, 302)           ~ "Redirect zonder creator",
    str_detect(tolower(error),
               "timeout|timed out")        ~ "Timeout",
    str_detect(tolower(error),
               "connection|network")       ~ "Netwerkfout",
    is.na(status_code) & is.na(error)      ~ "Niet in creator_map",
    TRUE                                   ~ paste0("Overig (", status_code, ")")
  )
}

# ── Verwerk alle deelnemers ──────────────────────────────────
audit_list <- lapply(participants, function(pid) {
  
  path_urls <- sprintf("gen/temp/unique_video_urls_%s.csv", pid)
  path_map  <- sprintf("data/unique_urls/creator_map_final_%s.csv", pid)
  
  # Bestanden inlezen (overslaan als niet aanwezig)
  if (!file.exists(path_urls)) {
    message(sprintf("⚠ Bestand niet gevonden, overgeslagen: %s", path_urls))
    return(NULL)
  }
  if (!file.exists(path_map)) {
    message(sprintf("⚠ Bestand niet gevonden, overgeslagen: %s", path_map))
    return(NULL)
  }
  
  unique_urls <- read_csv(path_urls, show_col_types = FALSE)   # kolommen: url, video_id
  creator_map <- read_csv(path_map,  show_col_types = FALSE)   # kolommen: input_url, final_url,
  #   creator_username, status_code, error
  
  # Zorg dat kolomnamen kloppen
  creator_map <- creator_map %>%
    rename(url = input_url)
  
  # Koppel unique_urls aan creator_map op URL
  joined <- unique_urls %>%
    left_join(creator_map %>%
                select(url, final_url, creator_username,
                       status_code, error),
              by = "url") %>%
    mutate(
      participant = pid,
      reden       = categorise_failure(status_code, error, creator_username)
    )
  
  joined
})

# Combineer
audit_all <- bind_rows(audit_list)

# ── Samenvatting per deelnemer ───────────────────────────────
cat("\n")
cat(strrep("=", 60), "\n")
cat("  AUDIT: ONTBREKENDE CREATOR DATA PER DEELNEMER\n")
cat(strrep("=", 60), "\n\n")

summary_per_participant <- audit_all %>%
  group_by(participant) %>%
  summarise(
    totaal_unieke_urls  = n(),
    met_creator         = sum(reden == "OK"),
    zonder_creator      = sum(reden != "OK"),
    pct_zonder          = round(100 * zonder_creator / totaal_unieke_urls, 1),
    .groups = "drop"
  )

for (i in seq_len(nrow(summary_per_participant))) {
  r <- summary_per_participant[i, ]
  cat(sprintf("  %s\n", toupper(r$participant)))
  cat(sprintf("    Totaal unieke video-URL's : %d\n",  r$totaal_unieke_urls))
  cat(sprintf("    Met creator-data          : %d\n",  r$met_creator))
  cat(sprintf("    Zonder creator-data       : %d  (%.1f%%)\n",
              r$zonder_creator, r$pct_zonder))
  cat("\n")
}

# ── Uitsplitsing naar reden (alle deelnemers samen) ──────────
cat(strrep("-", 60), "\n")
cat("  UITSPLITSING NAAR REDEN (alle deelnemers gecombineerd)\n")
cat(strrep("-", 60), "\n\n")

reden_totaal <- audit_all %>%
  count(reden, name = "n_videos") %>%
  mutate(pct = round(100 * n_videos / sum(n_videos), 1)) %>%
  arrange(desc(n_videos))

for (i in seq_len(nrow(reden_totaal))) {
  r <- reden_totaal[i, ]
  cat(sprintf("  %-35s %5d  (%5.1f%%)\n", r$reden, r$n_videos, r$pct))
}
cat("\n")

# ── Uitsplitsing naar reden PER deelnemer ───────────────────
cat(strrep("-", 60), "\n")
cat("  UITSPLITSING NAAR REDEN PER DEELNEMER\n")
cat(strrep("-", 60), "\n\n")

reden_per_participant <- audit_all %>%
  count(participant, reden, name = "n_videos") %>%
  group_by(participant) %>%
  mutate(pct = round(100 * n_videos / sum(n_videos), 1)) %>%
  ungroup() %>%
  arrange(participant, desc(n_videos))

for (pid in participants) {
  sub <- reden_per_participant %>% filter(participant == pid)
  if (nrow(sub) == 0) next
  cat(sprintf("  %s:\n", toupper(pid)))
  for (i in seq_len(nrow(sub))) {
    cat(sprintf("    %-33s %5d  (%5.1f%%)\n",
                sub$reden[i], sub$n_videos[i], sub$pct[i]))
  }
  cat("\n")
}

# ── Totaalregel ─────────────────────────────────────────────
totaal <- audit_all %>%
  summarise(
    totaal        = n(),
    met_creator   = sum(reden == "OK"),
    zonder        = sum(reden != "OK"),
    pct_zonder    = round(100 * zonder / totaal, 1),
    pct_verwijderd = round(100 * sum(reden == "Verwijderd (404)") / totaal, 1)
  )

cat(strrep("=", 60), "\n")
cat("  TOTAAL (alle deelnemers)\n")
cat(strrep("=", 60), "\n")
cat(sprintf("  Totaal unieke video-URL's : %d\n",    totaal$totaal))
cat(sprintf("  Met creator-data          : %d\n",    totaal$met_creator))
cat(sprintf("  Zonder creator-data       : %d  (%.1f%%)\n",
            totaal$zonder, totaal$pct_zonder))
cat(sprintf("  Waarvan verwijderd (404)  : %d  (%.1f%% van totaal)\n",
            sum(audit_all$reden == "Verwijderd (404)"),
            totaal$pct_verwijderd))
cat(strrep("=", 60), "\n\n")

# ── Exporteer volledige auditdata ────────────────────────────
dir.create("gen/temp", showWarnings = FALSE, recursive = TRUE)

audit_export <- audit_all %>%
  select(participant, video_id, url, final_url,
         creator_username, status_code, error, reden)

write_csv(audit_export, "gen/temp/deleted_videos_audit.csv")
cat("✓ Auditbestand opgeslagen: gen/temp/deleted_videos_audit.csv\n\n")