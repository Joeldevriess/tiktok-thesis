library(fixest)
library(dplyr)
library(readr)
library(knitr)

panel <- read_csv("gen/analysis/weekly_panel_estimation.csv") %>%
  mutate(
    deelnemer = as.factor(deelnemer),
    week      = as.Date(week),
        log_zoek  = log1p(zoek_intensiteit)
  )

#DV 1: SESSION DURATION

m1_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek,
  data = panel,
  vcov = ~deelnemer 
)

m2_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek | week,
  data = panel,
  vcov = ~deelnemer  
)

m3_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

# DV 2: LIKE-RATE

m4_like <- feols(
  like_rate ~ discovery_ratio * log_zoek,
  data = panel,
  vcov = ~deelnemer
)

m5_like <- feols(
  like_rate ~ discovery_ratio * log_zoek | week,
  data = panel,
  vcov = ~deelnemer
)

m6_like <- feols(
  like_rate ~ discovery_ratio * log_zoek | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

# REGRESSION TABLES

etable(
  m1_sess, m2_sess, m3_sess,
  headers = c(
    "Model 1: OLS",
    "Model 2: Time FE",
    "Model 3: TWFE"
  ),
  dict = c(
    "discovery_ratio" = "Discovery Ratio",
    "log_zoek" = "log(Search Intensity + 1)",
    "discovery_ratio:log_zoek" = "DR × log(Search)"
  ),
  fitstat     = c("n", "r2", "wr2"),  
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  se.below    = TRUE,
  title       = "Dependent Variable: Session Duration (minutes per week)"
)

etable(
  m4_like, m5_like, m6_like,
  headers = c(
    "Model 1: OLS",
    "Model 2: Time FE",
    "Model 3: TWFE"
  ),
  dict = c(
    "discovery_ratio" = "Discovery Ratio",
    "log_zoek" = "log(Search Intensity + 1)",
    "discovery_ratio:log_zoek" = "DR × log(Search)"
  ),
  fitstat     = c("n", "r2", "wr2"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  se.below    = TRUE,
  title       = "Dependent Variable: Like-Rate (likes per video viewed)"
)