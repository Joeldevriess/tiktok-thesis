library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)
library(patchwork)

panel <- read_csv("gen/analysis/weekly_panel_estimation.csv",
                  show_col_types = FALSE) %>%
  mutate(deelnemer = as.factor(deelnemer))

cat("Participants:               ", n_distinct(panel$deelnemer), "\n")
cat("Person-week observations:   ", nrow(panel), "\n\n")

participant_summary <- panel %>%
  group_by(deelnemer) %>%
  summarise(
    n_weeks                = n(),
    mean_discovery_ratio   = mean(discovery_ratio,       na.rm = TRUE),
    mean_session_duration  = mean(gem_sessieduur_mins,   na.rm = TRUE),
    mean_like_rate         = mean(like_rate,             na.rm = TRUE),
    mean_search_intensity  = mean(zoek_intensiteit,      na.rm = TRUE),
    mean_videos_per_week   = mean(n_videos_totaal,       na.rm = TRUE),
    mean_sessions_per_week = mean(n_sessies,             na.rm = TRUE),
    .groups = "drop"
  )

cat("Participant-level summary (17 rows):\n")
print(participant_summary, n = 20)
cat("\n")

dr_median <- median(participant_summary$mean_discovery_ratio)
cat(sprintf("Median participant-level Discovery Ratio: %.3f\n\n",
            dr_median))

participant_summary <- participant_summary %>%
  mutate(
    dr_group = if_else(mean_discovery_ratio >= dr_median,
                       "High DR", "Low DR"),
    dr_group = factor(dr_group, levels = c("Low DR", "High DR"))
  )

cat("Group sizes:\n")
print(table(participant_summary$dr_group))
cat("\n")

vars_to_compare <- c(
  "mean_discovery_ratio"   = "Discovery Ratio (mean)",
  "mean_session_duration"  = "Session Duration (mins)",
  "mean_like_rate"         = "Like-Rate",
  "mean_search_intensity"  = "Search Intensity (queries)",
  "mean_videos_per_week"   = "Videos per Week",
  "mean_sessions_per_week" = "Sessions per Week",
  "n_weeks"                = "Weeks Observed"
)

compare_groups <- function(var_name, var_label) {
  high_vals <- participant_summary %>%
    filter(dr_group == "High DR") %>%
    pull(!!sym(var_name))
  low_vals  <- participant_summary %>%
    filter(dr_group == "Low DR") %>%
    pull(!!sym(var_name))
  
  wt <- suppressWarnings(
    wilcox.test(high_vals, low_vals, exact = FALSE)
  )
  
  tibble(
    Variable      = var_label,
    `Low DR (M)`  = round(mean(low_vals,  na.rm = TRUE), 3),
    `High DR (M)` = round(mean(high_vals, na.rm = TRUE), 3),
    Difference    = round(mean(high_vals, na.rm = TRUE) -
                            mean(low_vals, na.rm = TRUE), 3),
    `W stat`      = round(wt$statistic, 1),
    `p-value`     = round(wt$p.value, 3)
  )
}

comparison_table <- map2_dfr(
  names(vars_to_compare),
  vars_to_compare,
  compare_groups
)

cat("Group comparison (Mann-Whitney):\n")
print(comparison_table)
cat("\n")

write_csv(comparison_table,
          "gen/analysis/table_high_low_dr_comparison.csv")

cor_vars <- setdiff(names(vars_to_compare), "mean_discovery_ratio")

cor_results <- map_dfr(cor_vars, function(v) {
  ct <- suppressWarnings(
    cor.test(participant_summary$mean_discovery_ratio,
             participant_summary[[v]],
             method = "spearman",
             exact  = FALSE)
  )
  tibble(
    Variable       = vars_to_compare[[v]],
    `Spearman rho` = round(unname(ct$estimate), 3),
    `p-value`      = round(ct$p.value, 3)
  )
})

cat("Spearman correlations of participant-level DR with outcomes:\n")
print(cor_results)
cat("\n")

write_csv(cor_results,
          "gen/analysis/table_dr_spearman_correlations.csv")

plot_vars <- setdiff(names(vars_to_compare), "mean_discovery_ratio")

# Plot A: boxplots by group

plot_data_a <- participant_summary %>%
  select(deelnemer, dr_group, all_of(plot_vars)) %>%
  pivot_longer(cols = -c(deelnemer, dr_group),
               names_to  = "variable",
               values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = plot_vars,
                           labels = vars_to_compare[plot_vars]))

plot_a <- ggplot(plot_data_a,
                 aes(x = dr_group, y = value, fill = dr_group)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.55) +
  geom_jitter(width = 0.12, size = 2, alpha = 0.8, colour = "grey20") +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("Low DR"  = "#3498DB",
                               "High DR" = "#E67E22")) +
  labs(
    title    = "Participant-Level Comparisons: High vs Low Discovery Ratio",
    subtitle = "Each point is one participant's mean across their observation weeks",
    x = NULL, y = "Value"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("gen/analysis/plot_high_low_dr_boxplots.png", plot_a,
       width = 11, height = 7, dpi = 200)

# Plot B: scatter of mean DR vs each variable

plot_data_b <- participant_summary %>%
  select(deelnemer, mean_discovery_ratio, all_of(plot_vars)) %>%
  pivot_longer(cols = -c(deelnemer, mean_discovery_ratio),
               names_to  = "variable",
               values_to = "value") %>%
  mutate(variable = factor(variable,
                           levels = plot_vars,
                           labels = vars_to_compare[plot_vars]))

plot_b <- ggplot(plot_data_b,
                 aes(x = mean_discovery_ratio, y = value)) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "#E67E22", fill = "#E67E22",
              alpha = 0.15, linewidth = 0.8, formula = y ~ x) +
  geom_point(size = 2.4, alpha = 0.85, colour = "#2C3E50") +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(
    title    = "Participant-Level Gradient: Discovery Ratio vs Outcomes",
    subtitle = "Each point is one participant (n = 17). Linear fit shown for visual reference only.",
    x = "Mean Discovery Ratio (per participant)",
    y = "Mean value (per participant)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave("gen/analysis/plot_high_low_dr_scatter.png", plot_b,
       width = 11, height = 7, dpi = 200)

# Combined panel (boxplots above, scatter below)

combined <- plot_a / plot_b +
  plot_annotation(
    tag_levels = "A",
    title    = "Between-Person Differences in Discovery Ratio",
    subtitle = "Panel A: median split. Panel B: continuous gradient across all 17 participants."
  )

ggsave("gen/analysis/plot_high_low_dr_combined.png", combined,
       width = 11, height = 13, dpi = 200)