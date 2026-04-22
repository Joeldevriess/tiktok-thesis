# Installeer fixest als je dat nog niet hebt

install.packages("fixest")

library(fixest)
library(dplyr)
library(readr)

panel <- read_csv("gen/analysis/weekly_panel_estimation.csv") %>%
  mutate(
    deelnemer = as.factor(deelnemer),
    week      = as.Date(week),
    # log1p-transformatie van Search Intensity vanwege sterke rechtsscheefheid
    # (max = 172, SD = 26.6, sterk beinvloed door uitschieters).
    # log1p(x) = log(x + 1) voorkomt problemen bij weken met 0 zoekopdrachten.
    log_zoek  = log1p(zoek_intensiteit)
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
  gem_sessieduur_mins ~ discovery_ratio * log_zoek,
  data = panel
)

# M3: TWFE zonder moderatie — Driscoll-Kraay (small N, large T)
m3_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio
  | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

# M4: TWFE + moderatie (hoofdmodel voor H1 en H2) — Driscoll-Kraay
m4_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek
  | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
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
  like_rate ~ discovery_ratio * log_zoek,
  data = panel
)

# M7: TWFE zonder moderatie — Driscoll-Kraay (small N, large T)
m7_like <- feols(
  like_rate ~ discovery_ratio
  | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

# M8: TWFE + moderatie (hoofdmodel voor E1 en H3) — Driscoll-Kraay
m8_like <- feols(
  like_rate ~ discovery_ratio * log_zoek
  | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
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
# ============================================================
# CONTROLE: DK vs. Clustered SE voor TWFE-modellen
# Laat zien hoe groot het verschil is in SE en significantie
# ============================================================

cat("\n══ CONTROLE: M4 Session Duration ═══════════════════════\n")
cat("\nDriscoll-Kraay (gebruikte specificatie):\n")
summary(m4_sess, vcov = "DK")
cat("\nClustered op deelnemer (ter vergelijking):\n")
summary(m4_sess, vcov = ~deelnemer)

cat("\n══ CONTROLE: M8 Like-Rate ══════════════════════════════\n")
cat("\nDriscoll-Kraay (gebruikte specificatie):\n")
summary(m8_like, vcov = "DK")
cat("\nClustered op deelnemer (ter vergelijking):\n")
summary(m8_like, vcov = ~deelnemer)

# ============================================================
# CORRELATIETABEL
# ============================================================
# Pearson correlaties tussen de vier onderzoeksvariabelen.
# Search Intensity wordt hier zowel als ruwe count als
# log-getransformeerde versie gerapporteerd voor vergelijking.
# In de modellen wordt uitsluitend log_zoek gebruikt.

library(knitr)

corr_vars <- panel %>%
  select(
    `Discovery Ratio`  = discovery_ratio,
    `Session Duration` = gem_sessieduur_mins,
    `Like-Rate`        = like_rate,
    `Search Intensity` = zoek_intensiteit
  )

corr_matrix <- cor(corr_vars, use = "pairwise.complete.obs", method = "pearson")

cat("\n══ Pearson correlatietabel (estimation sample) ════════\n")
print(round(corr_matrix, 3))

# Nette tabel met knitr (handig voor Rmd of directe output)
cat("\n")
kable(
  round(corr_matrix, 3),
  caption = "Pearson correlations between research variables (estimation sample, N = 1,029)",
  format  = "simple"
)