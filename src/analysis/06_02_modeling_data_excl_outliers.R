library(fixest)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ggplot2)
library(knitr)

panel_raw <- read_csv("gen/analysis/weekly_panel_estimation.csv",
                      show_col_types = FALSE) %>%
  mutate(
    deelnemer = as.factor(deelnemer),
    week      = as.Date(week),
    log_zoek  = log1p(zoek_intensiteit)
  )

cat("Baseline observations:    ", nrow(panel_raw), "\n")
cat("Baseline participants:    ", n_distinct(panel_raw$deelnemer), "\n\n")

p99_sess  <- quantile(panel_raw$gem_sessieduur_mins, 0.99, na.rm = TRUE)
p99_like  <- quantile(panel_raw$like_rate,           0.99, na.rm = TRUE)
p99_zoek  <- quantile(panel_raw$zoek_intensiteit,    0.99, na.rm = TRUE)

cat("99th-percentile thresholds (computed on full sample):\n")
cat(sprintf("  Session Duration (mins):  %.2f\n", p99_sess))
cat(sprintf("  Like-Rate:                %.4f\n", p99_like))
cat(sprintf("  Search Intensity:         %.0f\n", p99_zoek))
cat("\n")

panel_raw <- panel_raw %>%
  mutate(
    is_outlier_obs = (gem_sessieduur_mins > p99_sess) |
      (like_rate            > p99_like) |
      (zoek_intensiteit     > p99_zoek)
  )

cat(sprintf("Observations flagged as >P99 on any key variable: %d (%.1f%%)\n\n",
            sum(panel_raw$is_outlier_obs, na.rm = TRUE),
            100 * mean(panel_raw$is_outlier_obs, na.rm = TRUE)))

samples <- list(
  "S1 Full sample"     = panel_raw,
  "S2 Excl. p06"       = panel_raw %>%
    filter(deelnemer != "p06"),
  "S3 Excl. obs > P99" = panel_raw %>%
    filter(!is_outlier_obs)
)

cat("Sample sizes by specification:\n")
walk2(names(samples), samples, function(nm, df) {
  cat(sprintf("  %-25s  N = %4d obs, %2d participants\n",
              nm, nrow(df), n_distinct(df$deelnemer)))
})
cat("\n")

estimate_twfe <- function(df) {
  m_sess <- feols(
    gem_sessieduur_mins ~ discovery_ratio * log_zoek |
      deelnemer + week,
    panel.id = ~deelnemer + week,
    vcov     = "DK",
    data     = df
  )
  m_like <- feols(
    like_rate ~ discovery_ratio * log_zoek |
      deelnemer + week,
    panel.id = ~deelnemer + week,
    vcov     = "DK",
    data     = df
  )
  list(session = m_sess, like = m_like)
}

models <- map(samples, estimate_twfe)

extract_coefs <- function(model, sample_name, dv_label) {
  ct <- coeftable(model)
  tibble(
    Sample      = sample_name,
    DV          = dv_label,
    Term        = rownames(ct),
    Estimate    = ct[, "Estimate"],
    Std.Error   = ct[, "Std. Error"],
    t.value     = ct[, "t value"],
    p.value     = ct[, "Pr(>|t|)"]
  )
}

results <- imap_dfr(models, function(m, nm) {
  bind_rows(
    extract_coefs(m$session, nm, "Session Duration"),
    extract_coefs(m$like,    nm, "Like-Rate")
  )
})

# Tidy term labels
results <- results %>%
  mutate(Term = recode(Term,
                       "discovery_ratio"           = "Discovery Ratio",
                       "log_zoek"                  = "log(Search + 1)",
                       "discovery_ratio:log_zoek"  = "DR x log(Search)"
  ))

# Save tidy long-format output
write_csv(results, "gen/analysis/table_robustness_long.csv")

format_estimate <- function(est, se, p) {
  star <- case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    p < .10  ~ ".",
    TRUE     ~ ""
  )
  sprintf("%.3f%s (%.3f)", est, star, se)
}

make_compact_table <- function(dv_label) {
  results %>%
    filter(DV == dv_label) %>%
    mutate(cell = format_estimate(Estimate, Std.Error, p.value)) %>%
    select(Sample, Term, cell) %>%
    pivot_wider(names_from = Term, values_from = cell) %>%
    left_join(
      tibble(
        Sample = names(samples),
        N      = map_int(samples, nrow)
      ),
      by = "Sample"
    )
}

table_sess <- make_compact_table("Session Duration")
table_like <- make_compact_table("Like-Rate")

cat("\nROBUSTNESS TABLE: Session Duration (TWFE coefficients)\n")
cat("══════════════════════════════════════════════════════════════\n")
print(kable(table_sess, format = "simple"))
cat("\n")

cat("\nROBUSTNESS TABLE: Like-Rate (TWFE coefficients)\n")
cat("══════════════════════════════════════════════════════════════\n")
print(kable(table_like, format = "simple"))
cat("\n")

write_csv(table_sess, "gen/analysis/table_robustness_session.csv")
write_csv(table_like, "gen/analysis/table_robustness_like.csv")

plot_data <- results %>%
  filter(Term == "Discovery Ratio") %>%
  mutate(
    ci_lo = Estimate - 1.96 * Std.Error,
    ci_hi = Estimate + 1.96 * Std.Error,
    Sample = factor(Sample, levels = names(samples))
  )

p_robust <- ggplot(plot_data,
                   aes(x = Sample, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                width = 0.18, linewidth = 0.7,
                colour = "#2C3E50") +
  geom_point(size = 3, colour = "#E67E22") +
  facet_wrap(~DV, scales = "free_y") +
  labs(
    title    = "Discovery Ratio Coefficient Across Outlier Specifications",
    subtitle = "TWFE estimates with 95% Driscoll-Kraay confidence intervals",
    x        = NULL,
    y        = "Discovery Ratio coefficient"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 25, hjust = 1),
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("gen/analysis/plot_robustness_coefs.png", p_robust,
       width = 10, height = 5, dpi = 200)

stability <- results %>%
  mutate(sig05 = p.value < .05) %>%
  group_by(DV, Term) %>%
  summarise(
    n_specs            = n(),
    n_sig              = sum(sig05),
    sig_consistent     = n_sig %in% c(0, n_specs),
    .groups = "drop"
  )

cat("\nSTABILITY CHECK: Does significance at p < .05 change across specs?\n")
cat("══════════════════════════════════════════════════════════════\n")
print(kable(stability, format = "simple"))
cat("\n")
cat("Interpretation: sig_consistent = TRUE means the inferential\n")
cat("  conclusion (significant or not) does not change across the\n")
cat("  three outlier specifications. FALSE means at least one\n")
cat("  specification yields a different conclusion and warrants\n")
cat("  explicit discussion.\n\n")

# ============================================================
# OUTPUT SUMMARY
# ============================================================

cat("Outputs saved to gen/analysis/:\n")
cat("  - table_robustness_long.csv         (tidy long-format)\n")
cat("  - table_robustness_session.csv      (compact, Session Duration)\n")
cat("  - table_robustness_like.csv         (compact, Like-Rate)\n")
cat("  - plot_robustness_coefs.png         (DR coefficient across specs)\n")