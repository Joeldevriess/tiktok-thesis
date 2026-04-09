# Installeer fixest als je dat nog niet hebt

install.packages("fixest")

library(fixest)
library(dplyr)
library(readr)

panel <- read_csv("gen/analysis/weekly_panel_all.csv") %>%
  mutate(
    deelnemer = as.factor(deelnemer),
    week      = as.Date(week)
  )

# ============================================================
# DV 1: SESSION DURATION
# ============================================================

# M1: Baseline OLS
m1_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio,
  data = panel
)

# M2: OLS + moderatie (H2)
m2_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * zoek_intensiteit,
  data = panel
)

# M3: TWFE zonder moderatie
m3_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio
  | deelnemer + week,
  cluster = ~deelnemer,
  data    = panel
)

# M4: TWFE + moderatie (hoofdmodel voor H1 en H2)
m4_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * zoek_intensiteit
  | deelnemer + week,
  cluster = ~deelnemer,
  data    = panel
)

# ============================================================
# DV 2: LIKE-RATE
# ============================================================

# M5: Baseline OLS
m5_like <- feols(
  like_rate ~ discovery_ratio,
  data = panel
)

# M6: OLS + moderatie (H3)
m6_like <- feols(
  like_rate ~ discovery_ratio * zoek_intensiteit,
  data = panel
)

# M7: TWFE zonder moderatie
m7_like <- feols(
  like_rate ~ discovery_ratio
  | deelnemer + week,
  cluster = ~deelnemer,
  data    = panel
)

# M8: TWFE + moderatie (hoofdmodel voor E1 en H3)
m8_like <- feols(
  like_rate ~ discovery_ratio * zoek_intensiteit
  | deelnemer + week,
  cluster = ~deelnemer,
  data    = panel
)

# ============================================================
# TABELLEN — headers werken met dict() in fixest
# ============================================================

# Tabel 1: Session Duration
etable(
  m1_sess, m2_sess, m3_sess, m4_sess,
  headers = c(
    "OLS Baseline",
    "OLS + Moderatie",
    "TWFE Baseline",
    "TWFE + Moderatie"
  ),
  fitstat     = c("n", "r2", "ar2"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  se.below    = TRUE,
  title       = "Session Duration"
)

# Tabel 2: Like-Rate
etable(
  m5_like, m6_like, m7_like, m8_like,
  headers = c(
    "OLS Baseline",
    "OLS + Moderatie",
    "TWFE Baseline",
    "TWFE + Moderatie"
  ),
  fitstat     = c("n", "r2", "ar2"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  se.below    = TRUE,
  title       = "Like-Rate"
)