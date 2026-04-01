# ============================================================
# 04_descriptive_plots.R
#
# Wat doet dit script?
#   Maakt twee soorten beschrijvende plots voor de vier
#   onderzoeksvariabelen:
#     1. Tijdlijnplots  – verloop per deelnemer over de weken
#     2. Verdelingsplots – boxplots om uitbijters te spotten
#
# Vereisten:
#   - Script 03 is gedraaid → gen/analysis/weekly_panel_all.csv bestaat
# ============================================================

library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

# ============================================================
# STAP 1: Data inladen
# ============================================================
# We lezen het gecombineerde weekpanel in dat script 03 heeft
# opgeslagen. Zo zijn deze plots volledig onafhankelijk van de
# andere scripts en altijd opnieuw te draaien.

panel_all <- read_csv("gen/analysis/weekly_panel_all.csv",
                      show_col_types = FALSE)

cat("Panel geladen:", nrow(panel_all), "rijen,",
    n_distinct(panel_all$deelnemer), "deelnemers\n")

# ============================================================
# STAP 2: Data voorbereiden voor de plots
# ============================================================
# We zetten de vier variabelen in "long format": in plaats van
# vier losse kolommen krijgen we twee kolommen — één met de
# variabelenaam en één met de waarde. Dit maakt het makkelijk
# om met facet_wrap() vier plots tegelijk te maken.
#
# We geven de variabelen ook leesbare Nederlandse labels mee
# zodat de plottikets begrijpelijk zijn.

variabele_labels <- c(
  discovery_ratio      = "Discovery Ratio\n(aandeel nieuwe creators)",
  gem_sessieduur_mins  = "Gem. Sessieduur\n(minuten)",
  like_rate            = "Like-Rate\n(likes / bekeken video's)",
  zoek_intensiteit     = "Zoekintensiteit\n(aantal zoekopdrachten)"
)

panel_long <- panel_all %>%
  select(deelnemer, week,
         discovery_ratio, gem_sessieduur_mins,
         like_rate, zoek_intensiteit) %>%
  pivot_longer(
    cols      = c(discovery_ratio, gem_sessieduur_mins,
                  like_rate, zoek_intensiteit),
    names_to  = "variabele",
    values_to = "waarde"
  ) %>%
  mutate(
    # Vervang de technische kolomnaam door het leesbare label
    variabele = factor(variabele,
                       levels = names(variabele_labels),
                       labels = variabele_labels)
  )

# ============================================================
# STAP 3: Plot 1 — Tijdlijn per deelnemer
# ============================================================
# Wat zie je hier?
#   Elke lijn is één deelnemer. Je kunt zo in één oogopslag zien:
#   - Of er trends zijn over de tijd (stijgt/daalt een variabele?)
#   - Of bepaalde deelnemers extreem afwijken van de rest
#   - Of er weken zijn met verdachte pieken of dalen
#
# facet_wrap() maakt automatisch een apart paneel per variabele.
# scales = "free_y" zorgt dat elke variabele zijn eigen y-as
# krijgt — anders zijn kleine variabelen niet zichtbaar naast
# grote.

plot_tijdlijn <- ggplot(panel_long,
                        aes(x = week, y = waarde,
                            colour = deelnemer, group = deelnemer)) +
  geom_line(alpha = 0.7, linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.5) +
  facet_wrap(~variabele, scales = "free_y", ncol = 2) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Verloop van de onderzoeksvariabelen per deelnemer",
    subtitle = "Elke lijn = één deelnemer | Wekelijkse waarden",
    x        = NULL,
    y        = "Waarde",
    colour   = "Deelnemer"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  )

# Opslaan als PNG (breed formaat zodat de vier panelen goed passen)
ggsave("gen/analysis/plot1_tijdlijn.png",
       plot   = plot_tijdlijn,
       width  = 14, height = 9, dpi = 150)

cat("✓ Plot 1 opgeslagen: gen/analysis/plot1_tijdlijn.png\n")

# Toon de plot ook in RStudio
print(plot_tijdlijn)

# ============================================================
# STAP 4: Plot 2 — Verdelingsplots (boxplots)
# ============================================================
# Wat zie je hier?
#   Per variabele één boxplot over alle deelnemers en weken:
#   - De box toont het middelste 50% van de waarden (IQR)
#   - De lijn in de box is de mediaan
#   - De "whiskers" gaan tot 1.5× IQR
#   - Punten buiten de whiskers zijn uitbijters — die zijn
#     interessant om nader te bekijken
#
# Waarom ook een jitter-laag?
#   Boxplots verbergen hoeveel datapunten er zijn. Door de
#   individuele punten half-transparant erop te leggen zie je
#   zowel de verdeling als de dichtheid van de data.

plot_verdeling <- ggplot(panel_long,
                         aes(x = variabele, y = waarde)) +
  geom_boxplot(outlier.shape = NA,           # uitbijters via jitter, niet dubbel
               fill = "#D0E8F2", colour = "#2C7BB6",
               width = 0.5, linewidth = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.25,
              size = 0.9, colour = "#2C7BB6") +
  facet_wrap(~variabele, scales = "free", ncol = 2) +
  labs(
    title    = "Verdeling van de onderzoeksvariabelen",
    subtitle = "Alle deelnemers × alle weken | Punten buiten de whiskers zijn uitbijters",
    x        = NULL,
    y        = "Waarde"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_blank(),   # variabelenaam staat al in het paneel
    axis.ticks.x     = element_blank(),
    panel.grid.minor = element_blank()
  )

# Opslaan
ggsave("gen/analysis/plot2_verdeling.png",
       plot   = plot_verdeling,
       width  = 12, height = 8, dpi = 150)

cat("✓ Plot 2 opgeslagen: gen/analysis/plot2_verdeling.png\n")

# Toon de plot ook in RStudio
print(plot_verdeling)

# ============================================================
# STAP 5: Snelle uitbijtercheck in de console
# ============================================================
# Naast de visuele check printen we ook welke deelnemer-week
# combinaties boven de 99e percentiel zitten per variabele.
# Zo kun je gericht kijken of die waarden kloppen of data-fouten zijn.

cat("\n══ Uitbijters boven 99e percentiel ══════════════════\n")

panel_all %>%
  select(deelnemer, week,
         discovery_ratio, gem_sessieduur_mins,
         like_rate, zoek_intensiteit) %>%
  pivot_longer(-c(deelnemer, week),
               names_to = "variabele", values_to = "waarde") %>%
  group_by(variabele) %>%
  mutate(p99 = quantile(waarde, 0.99, na.rm = TRUE)) %>%
  filter(waarde > p99) %>%
  select(variabele, deelnemer, week, waarde, p99) %>%
  arrange(variabele, desc(waarde)) %>%
  print(n = 50)