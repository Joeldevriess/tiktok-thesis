# ============================================================
# FORMATTED TABLES voor Word (Tables 7 en 8)
# ============================================================
# Dit script genereert netjes opgemaakte tabellen die je
# kunt copy-pasten naar Word.

library(fixest)
library(dplyr)

# Verondersteld dat je modellen al geladen zijn

# ============================================================
# TABLE 7: Session Duration Models
# ============================================================

cat("\n══════════════════════════════════════════════════════════\n")
cat("TABLE 7: Effect of Discovery Ratio and Search Intensity on Session Duration\n")
cat("══════════════════════════════════════════════════════════\n\n")

etable(
  m1_sess, m2_sess, m3_sess, m4_sess,
  headers = c(
    "Model 1",
    "Model 2",
    "Model 3",
    "Model 4"
  ),
  fitstat = c("n", "r2", "ar2", "wr2"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  se.below = TRUE,
  title = "Table 7. Effect of Discovery Ratio and Search Intensity on Session Duration",
  dict = c(
    discovery_ratio = "Discovery Ratio",
    log_zoek = "Search Intensity (log)",
    "discovery_ratio:log_zoek" = "Discovery Ratio × Search Intensity"
  )
)

cat("\nNote: Models 1-2 report IID standard errors. Models 3-4 report Driscoll-Kraay standard errors")
cat("\nwith a maximum lag of 2. One observation was removed as a fixed-effect singleton.")
cat("\n*** p < .001, ** p < .01, * p < .05\n")

# ============================================================
# TABLE 8: Like-Rate Models
# ============================================================

cat("\n\n══════════════════════════════════════════════════════════\n")
cat("TABLE 8: Effect of Discovery Ratio and Search Intensity on Like-Rate\n")
cat("══════════════════════════════════════════════════════════\n\n")

etable(
  m5_like, m6_like, m7_like, m8_like,
  headers = c(
    "Model 5",
    "Model 6",
    "Model 7",
    "Model 8"
  ),
  fitstat = c("n", "r2", "ar2", "wr2"),
  signif.code = c("***" = 0.001, "**" = 0.01, "*" = 0.05),
  se.below = TRUE,
  title = "Table 8. Effect of Discovery Ratio and Search Intensity on Like-Rate",
  dict = c(
    discovery_ratio = "Discovery Ratio",
    log_zoek = "Search Intensity (log)",
    "discovery_ratio:log_zoek" = "Discovery Ratio × Search Intensity"
  )
)

cat("\nNote: Models 5-6 report IID standard errors. Models 7-8 report Driscoll-Kraay standard errors")
cat("\nwith a maximum lag of 2. One observation was removed because of missing Like-Rate data.")
cat("\nOne observation was removed as a fixed-effect singleton.")
cat("\n*** p < .001, ** p < .01, * p < .05\n")

# ============================================================
# EXTRA: Gedetailleerde model comparison metrics
# ============================================================

cat("\n\n══ Model Comparison Metrics ═══════════════════════════════\n")

comparison_sess <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Specification = c("OLS Baseline", "OLS + Moderation", "TWFE Baseline", "TWFE + Moderation"),
  N = c(
    nobs(m1_sess),
    nobs(m2_sess),
    nobs(m3_sess),
    nobs(m4_sess)
  ),
  R2 = c(
    r2(m1_sess),
    r2(m2_sess),
    r2(m3_sess),
    r2(m4_sess)
  ),
  Adj_R2 = c(
    r2(m1_sess, "ar2"),
    r2(m2_sess, "ar2"),
    r2(m3_sess, "ar2"),
    r2(m4_sess, "ar2")
  ),
  Within_R2 = c(
    NA,
    NA,
    r2(m3_sess, "wr2"),
    r2(m4_sess, "wr2")
  )
)

cat("\nSession Duration Models:\n")
comparison_sess_print <- comparison_sess
comparison_sess_print[, 3:6] <- round(comparison_sess[, 3:6], 4)
print(comparison_sess_print)

comparison_like <- data.frame(
  Model = c("Model 5", "Model 6", "Model 7", "Model 8"),
  Specification = c("OLS Baseline", "OLS + Moderation", "TWFE Baseline", "TWFE + Moderation"),
  N = c(
    nobs(m5_like),
    nobs(m6_like),
    nobs(m7_like),
    nobs(m8_like)
  ),
  R2 = c(
    r2(m5_like),
    r2(m6_like),
    r2(m7_like),
    r2(m8_like)
  ),
  Adj_R2 = c(
    r2(m5_like, "ar2"),
    r2(m6_like, "ar2"),
    r2(m7_like, "ar2"),
    r2(m8_like, "ar2")
  ),
  Within_R2 = c(
    NA,
    NA,
    r2(m7_like, "wr2"),
    r2(m8_like, "wr2")
  )
)

cat("\nLike-Rate Models:\n")
comparison_like_print <- comparison_like
comparison_like_print[, 3:6] <- round(comparison_like[, 3:6], 4)
print(comparison_like_print)