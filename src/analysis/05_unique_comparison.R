library(tidyverse)
library(kableExtra)
library(scales)

participant_stats <- panel_all %>%
  group_by(deelnemer) %>%
  summarise(
    n_weeks = n(),
    total_videos = sum(n_videos_totaal, na.rm = TRUE),
    avg_videos_per_week = mean(n_videos_totaal, na.rm = TRUE),
    median_videos_per_week = median(n_videos_totaal, na.rm = TRUE),
    sd_videos_per_week = sd(n_videos_totaal, na.rm = TRUE),
    min_videos_week = min(n_videos_totaal, na.rm = TRUE),
    max_videos_week = max(n_videos_totaal, na.rm = TRUE),
    
        avg_discovery_ratio = mean(discovery_ratio, na.rm = TRUE),
    sd_discovery_ratio = sd(discovery_ratio, na.rm = TRUE),
    
    total_mapped = sum(n_videos_gemapped, na.rm = TRUE),
    est_unique_creators = sum(n_videos_nieuwe_crea, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    pct_mapped = (total_mapped / total_videos) * 100,
    threshold_group = if_else(avg_videos_per_week >= 500, "≥500", "<500"),
    included = (avg_videos_per_week >= 500) & (n_weeks >= 10),
    excluded_reason = case_when(
      n_weeks < 10 & avg_videos_per_week < 500 ~ "Both criteria",
      n_weeks < 10 ~ "< 10 weeks",
      avg_videos_per_week < 500 ~ "< 500 videos/week",
      TRUE ~ "Included"
    )
  )

table_individual <- participant_stats %>%
  select(
    deelnemer, n_weeks, avg_videos_per_week, est_unique_creators,
    pct_mapped, avg_discovery_ratio, threshold_group, excluded_reason
  ) %>%
  arrange(desc(avg_videos_per_week)) %>%
  mutate(
    avg_videos_per_week = round(avg_videos_per_week, 1),
    pct_mapped = round(pct_mapped, 1),
    avg_discovery_ratio = round(avg_discovery_ratio, 3)
  )

print(table_individual, n = 25)

write_csv(table_individual, "output/table_sample_selection_threshold.csv")

summary_by_group <- participant_stats %>%
  group_by(threshold_group) %>%
  summarise(
    n_participants = n(),
    avg_weeks = mean(n_weeks),
    avg_videos_week = mean(avg_videos_per_week),
    avg_unique_creators = mean(est_unique_creators),
    avg_mapped_pct = mean(pct_mapped),
    avg_DR = mean(avg_discovery_ratio),
    sd_DR = sd(avg_discovery_ratio),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric) & !n_participants, ~round(.x, 2)))

print(summary_by_group)

p1 <- ggplot(participant_stats, aes(x = avg_videos_per_week)) +
  geom_histogram(bins = 15, fill = "#2C3E50", alpha = 0.8, color = "white") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "#E74C3C", 
             size = 1.2) +
  annotate("text", x = 500, y = Inf, 
           label = "Threshold = 500", 
           hjust = -0.1, vjust = 1.5, 
           color = "#E74C3C", size = 4.5, fontface = "bold") +
  annotate("text", x = 500, y = Inf,
           label = sprintf("(15th percentile)"),
           hjust = -0.1, vjust = 3,
           color = "#E74C3C", size = 3.5) +
  scale_x_continuous(labels = comma, breaks = seq(0, 8000, 1000)) +
  labs(
    title = "Distribution of Average Videos per Week",
    subtitle = "Three participants excluded for low activity (<500 videos/week)",
    x = "Average Videos per Week",
    y = "Number of Participants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("output/plot_videos_distribution.png", p1, 
       width = 10, height = 6, dpi = 300)

cor_value <- cor(participant_stats$avg_videos_per_week, 
                 participant_stats$est_unique_creators)

p2 <- ggplot(participant_stats, 
             aes(x = avg_videos_per_week, 
                 y = est_unique_creators,
                 color = threshold_group,
                 shape = threshold_group)) +
  geom_vline(xintercept = 500, linetype = "dashed", 
             color = "grey50", alpha = 0.5) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, 
              color = "grey30", linetype = "dotted", size = 0.8) +
  scale_color_manual(
    values = c("<500" = "#E74C3C", "≥500" = "#3498DB"),
    name = "Activity Level"
  ) +
  scale_shape_manual(
    values = c("<500" = 17, "≥500" = 16),
    name = "Activity Level"
  ) +
  scale_x_continuous(labels = comma, breaks = seq(0, 8000, 1000)) +
  scale_y_continuous(labels = comma) +
  annotate("text", x = max(participant_stats$avg_videos_per_week) * 0.7, 
           y = max(participant_stats$est_unique_creators) * 0.15,
           label = sprintf("r = %.3f", cor_value),
           size = 5, fontface = "bold", color = "grey30") +
  labs(
    title = "Unique Creators vs. Average Weekly Activity",
    subtitle = "Strong positive correlation validates activity threshold",
    x = "Average Videos per Week",
    y = "Estimated Unique Creators Encountered"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("output/plot_creators_vs_activity.png", p2, 
       width = 10, height = 6, dpi = 300)

p3 <- ggplot(participant_stats, 
             aes(x = avg_videos_per_week, 
                 y = avg_discovery_ratio,
                 size = n_weeks,
                 color = threshold_group,
                 shape = threshold_group)) +
  geom_vline(xintercept = 500, linetype = "dashed", 
             color = "grey50", alpha = 0.5) +
  geom_point(alpha = 0.7) +
  scale_color_manual(
    values = c("<500" = "#E74C3C", "≥500" = "#3498DB"),
    name = "Activity Level"
  ) +
  scale_shape_manual(
    values = c("<500" = 17, "≥500" = 16),
    name = "Activity Level"
  ) +
  scale_size_continuous(
    range = c(3, 10),
    name = "Weeks Observed",
    breaks = c(10, 30, 50, 65)
  ) +
  scale_x_continuous(labels = comma, breaks = seq(0, 8000, 1000)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  annotate("rect", 
           xmin = -Inf, xmax = 500, 
           ymin = 0.65, ymax = Inf,
           fill = "#E74C3C", alpha = 0.1) +
  annotate("text", 
           x = 250, y = 0.85,
           label = "Inflated DR\n(measurement artifact)",
           size = 3.5, color = "#E74C3C", fontface = "italic") +
  labs(
    title = "Discovery Ratio by Activity Level",
    subtitle = "Low-activity participants show artificially high DR due to thin viewing histories",
    x = "Average Videos per Week",
    y = "Average Discovery Ratio",
    caption = "Point size indicates number of weeks observed"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    legend.box = "vertical",
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(order = 1, override.aes = list(size = 4)),
    shape = guide_legend(order = 1, override.aes = list(size = 4)),
    size = guide_legend(order = 2)
  )

ggsave("output/plot_DR_reliability.png", p3, 
       width = 10, height = 7, dpi = 300)

library(patchwork)

combined_plot <- (p1 / p2) | p3

combined_plot <- combined_plot + 
  plot_annotation(
    title = "Sample Selection Threshold Analysis",
    subtitle = "Justification for 500 videos/week exclusion criterion",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "grey40")
    )
  )

ggsave("output/plot_combined_threshold_analysis.png", combined_plot,
       width = 16, height = 10, dpi = 300)