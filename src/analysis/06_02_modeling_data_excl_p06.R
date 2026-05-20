library(fixest)
library(dplyr)
library(readr)
library(knitr)

# ============================================================
# DATA LOADING & SAMPLE RESTRICTION
# ============================================================
# Note: p_06 is excluded from the analysis sample.
# Rationale: [TO BE DOCUMENTED - see Section 4.X of thesis].
# This exclusion is applied consistently across all models below.
# Sensitivity: full-sample results are reported as a robustness check
# in Appendix X to demonstrate that conclusions do not hinge on this
# exclusion.

EXCLUDED_PARTICIPANTS <- c("p06")

panel_raw <- read_csv("gen/analysis/weekly_panel_estimation.csv")

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  SAMPLE CONSTRUCTION\n")
cat("══════════════════════════════════════════════════════════════\n")
cat(sprintf("  Raw observations:           %d\n", nrow(panel_raw)))
cat(sprintf("  Raw unique participants:    %d\n",
            length(unique(panel_raw$deelnemer))))

panel <- panel_raw %>%
  filter(!deelnemer %in% EXCLUDED_PARTICIPANTS) %>%
  mutate(
    # droplevels() ensures p_06 is not retained as an empty factor level
    deelnemer = droplevels(as.factor(deelnemer)),
    week      = as.Date(week),
    # log1p transformation of Search Intensity (see Data chapter for rationale)
    log_zoek  = log1p(zoek_intensiteit)
  )

cat(sprintf("  Excluded participants:      %s\n",
            paste(EXCLUDED_PARTICIPANTS, collapse = ", ")))
cat(sprintf("  Observations after exclusion: %d\n", nrow(panel)))
cat(sprintf("  Participants after exclusion: %d\n",
            length(unique(panel$deelnemer))))
cat("\n")

cat("══════════════════════════════════════════════════════════════\n")
cat("  MODEL ESTIMATION - Progressive Robustness Checks\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# ============================================================
# DV 1: SESSION DURATION
# ============================================================

cat("DEPENDENT VARIABLE 1: Session Duration\n")
cat("──────────────────────────────────────────────────────────────\n\n")

# Model 1: OLS with moderation
# No fixed effects - pooled estimation
# Tests whether relationship exists at all in raw data
# Clustered SE on participant level
cat("  [1/3] Estimating OLS with moderation (clustered SE)...\n")
m1_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek,
  data = panel,
  vcov = ~deelnemer  # Clustered SE on participant
)

# Model 2: Time FE with moderation
# Controls for time-specific shocks (e.g., seasonal trends, platform changes)
# Does NOT control for person-level heterogeneity
# Tests whether effects hold after removing temporal confounds
# Clustered SE on participant level
cat("  [2/3] Estimating Time FE with moderation (clustered SE)...\n")
m2_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek | week,
  data = panel,
  vcov = ~deelnemer  # Clustered SE on participant
)

# Model 3: TWFE with moderation (PREFERRED SPECIFICATION)
# Controls for BOTH time-specific shocks AND person-level heterogeneity
# Identifies effects purely from within-person temporal variation
# Driscoll-Kraay SE accounts for small N, large T structure
cat("  [3/3] Estimating TWFE with moderation (Driscoll-Kraay SE)...\n")
m3_sess <- feols(
  gem_sessieduur_mins ~ discovery_ratio * log_zoek | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

cat("  ✓ Session Duration models complete\n\n")

# ============================================================
# DV 2: LIKE-RATE
# ============================================================

cat("DEPENDENT VARIABLE 2: Like-Rate\n")
cat("──────────────────────────────────────────────────────────────\n\n")

# Model 4: OLS with moderation
# Clustered SE on participant level
cat("  [1/3] Estimating OLS with moderation (clustered SE)...\n")
m4_like <- feols(
  like_rate ~ discovery_ratio * log_zoek,
  data = panel,
  vcov = ~deelnemer
)

# Model 5: Time FE with moderation
# Clustered SE on participant level
cat("  [2/3] Estimating Time FE with moderation (clustered SE)...\n")
m5_like <- feols(
  like_rate ~ discovery_ratio * log_zoek | week,
  data = panel,
  vcov = ~deelnemer
)

# Model 6: TWFE with moderation (PREFERRED SPECIFICATION)
cat("  [3/3] Estimating TWFE with moderation (Driscoll-Kraay SE)...\n")
m6_like <- feols(
  like_rate ~ discovery_ratio * log_zoek | deelnemer + week,
  panel.id = ~deelnemer + week,
  vcov     = "DK",
  data     = panel
)

cat("  ✓ Like-Rate models complete\n\n")

# ============================================================
# REGRESSION TABLES
# ============================================================

cat("\n══════════════════════════════════════════════════════════════\n")
cat("  REGRESSION TABLES\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Table 1: Session Duration progression
cat("Table 1: Session Duration - Progressive Model Specifications\n")
cat(sprintf("(Estimation sample: N = %d obs, %d participants; p_06 excluded)\n",
            nrow(panel), length(unique(panel$deelnemer))))
cat("──────────────────────────────────────────────────────────────\n\n")

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
  fitstat     = c("n", "r2", "wr2"),  # Added within R² for FE models
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05, "." = 0.10),
  se.below    = TRUE,
  title       = "Dependent Variable: Session Duration (minutes per week) - p_06 excluded"
)

cat("\n")

# Table 2: Like-Rate progression
cat("Table 2: Like-Rate - Progressive Model Specifications\n")
cat(sprintf("(Estimation sample: N = %d obs, %d participants; p_06 excluded)\n",
            nrow(panel), length(unique(panel$deelnemer))))
cat("──────────────────────────────────────────────────────────────\n\n")

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
  title       = "Dependent Variable: Like-Rate (likes per video viewed) - p_06 excluded"
)

# ============================================================
# MODEL COMPARISON DIAGNOSTICS
# ============================================================

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("  MODEL COMPARISON & DIAGNOSTICS\n")
cat("══════════════════════════════════════════════════════════════\n\n")

# Compare R² across specifications for Session Duration
cat("SESSION DURATION - Model Fit Comparison:\n")
cat("──────────────────────────────────────────────────────────────\n")
cat(sprintf("  Model 1 (OLS):     R² = %.4f\n", r2(m1_sess)))
cat(sprintf("  Model 2 (Time FE): R² = %.4f, Within R² = %.4f\n",
            r2(m2_sess), r2(m2_sess, type = "wr2")))
cat(sprintf("  Model 3 (TWFE):    R² = %.4f, Within R² = %.4f\n",
            r2(m3_sess), r2(m3_sess, type = "wr2")))
cat("\nInterpretation:\n")
cat("  - Drop in R² from OLS to FE models indicates that fixed effects\n")
cat("    absorb substantial variance (confirming their necessity)\n")
cat("  - Low within R² in TWFE reflects limited within-person variation\n")
cat("    in predictors (see variance decomposition in Chapter 4)\n\n")

# Compare R² across specifications for Like-Rate
cat("LIKE-RATE - Model Fit Comparison:\n")
cat("──────────────────────────────────────────────────────────────\n")
cat(sprintf("  Model 1 (OLS):     R² = %.4f\n", r2(m4_like)))
cat(sprintf("  Model 2 (Time FE): R² = %.4f, Within R² = %.4f\n",
            r2(m5_like), r2(m5_like, type = "wr2")))
cat(sprintf("  Model 3 (TWFE):    R² = %.4f, Within R² = %.4f\n",
            r2(m6_like), r2(m6_like, type = "wr2")))
cat("\n")

# ============================================================
# CORRELATION TABLE (for descriptives)
# ============================================================

cat("\n\n══════════════════════════════════════════════════════════════\n")
cat("  CORRELATION TABLE\n")
cat("══════════════════════════════════════════════════════════════\n\n")

corr_vars <- panel %>%
  select(
    `Discovery Ratio`  = discovery_ratio,
    `Session Duration` = gem_sessieduur_mins,
    `Like-Rate`        = like_rate,
    `Search Intensity` = zoek_intensiteit
  )

corr_matrix <- cor(corr_vars, use = "pairwise.complete.obs", method = "pearson")

print(kable(
  round(corr_matrix, 3),
  caption = sprintf("Pearson correlations (estimation sample, N = %d; p_06 excluded)",
                    nrow(panel)),
  format  = "simple"
))