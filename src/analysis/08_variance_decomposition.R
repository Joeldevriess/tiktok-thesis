library(tidyverse)

panel <- read_csv("gen/analysis/weekly_panel_estimation.csv", show_col_types = FALSE)

MIN_WEKEN          <- 10
MIN_VIDEOS_PER_WK  <- 500

deelnemer_filter <- panel %>%
  group_by(deelnemer) %>%
  summarise(
    n_weken          = n(),
    gem_videos_p_wk  = mean(n_videos_totaal, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(opgenomen = n_weken >= MIN_WEKEN & gem_videos_p_wk >= MIN_VIDEOS_PER_WK)

uitgesloten <- deelnemer_filter %>% filter(!opgenomen) %>% pull(deelnemer)

panel_est <- panel %>%
  filter(deelnemer %in% (deelnemer_filter %>% filter(opgenomen) %>% pull(deelnemer))) %>%
  mutate(like_rate = if_else(like_rate > 1, NA_real_, like_rate))

cat("Uitgesloten deelnemers:", paste(uitgesloten, collapse = ", "), "\n")
cat("Estimatiesample:", n_distinct(panel_est$deelnemer), "deelnemers,",
    nrow(panel_est), "observaties\n\n")

# --- Variabelen ---------------------------------------------------------------
vars_to_decompose <- c("discovery_ratio", "gem_sessieduur_mins",
                       "like_rate", "zoek_intensiteit")
var_labels        <- c("Discovery Ratio", "Session Duration (mins)",
                       "Like-Rate", "Search Intensity")

# --- Kernfunctie: one-way random-effects variantiedecompositie ----------------
# Levert variantiecomponenten op die optellen tot het totaal, plus de ICC.
calc_variance_decomp <- function(data, var_name) {
  
  d <- data %>%
    filter(!is.na(.data[[var_name]])) %>%
    transmute(deelnemer, y = .data[[var_name]])
  
  grand <- mean(d$y)
  N     <- nrow(d)
  
  groep <- d %>%
    group_by(deelnemer) %>%
    summarise(n_i = n(), mean_i = mean(y), .groups = "drop")
  k <- nrow(groep)
  
  # Sums of squares
  SSB <- sum(groep$n_i * (groep$mean_i - grand)^2)              # between
  SSW <- d %>%
    left_join(groep, by = "deelnemer") %>%
    summarise(s = sum((y - mean_i)^2)) %>% pull(s)              # within
  
  # Mean squares
  MSB <- SSB / (k - 1)
  MSW <- SSW / (N - k)
  
  # Gecorrigeerde gemiddelde groepsgrootte voor ongebalanceerd panel
  n0 <- (N - sum(groep$n_i^2) / N) / (k - 1)
  
  # Variantiecomponenten (method of moments)
  sigma_within  <- MSW
  sigma_between <- max((MSB - MSW) / n0, 0)
  sigma_total   <- sigma_between + sigma_within
  
  icc          <- sigma_between / sigma_total
  between_pct  <- icc * 100
  within_pct   <- (1 - icc) * 100
  
  tibble(
    variable        = var_name,
    sigma_between   = sigma_between,
    sigma_within    = sigma_within,
    sigma_total     = sigma_total,
    between_pct     = between_pct,
    within_pct      = within_pct,          # between_pct + within_pct == 100
    icc             = icc,
    within_over_between = sigma_within / sigma_between
  )
}

results <- map_df(vars_to_decompose, ~calc_variance_decomp(panel_est, .x)) %>%
  mutate(variable_label = var_labels[match(variable, vars_to_decompose)])

# --- Tabel voor Tabel 6 -------------------------------------------------------
variance_table <- results %>%
  transmute(
    Variable          = variable_label,
    `Between Var`     = round(sigma_between, 4),
    `Within Var`      = round(sigma_within, 4),
    `Total Var`       = round(sigma_total, 4),
    `Between %`       = round(between_pct, 1),
    `Within %`        = round(within_pct, 1),
    ICC               = round(icc, 3)
  )

cat("=== Gecorrigeerde variantiedecompositie (estimatiesample, n = 17) ===\n")
print(variance_table)

write_csv(variance_table, "table_variance_decomposition.csv")

plot_data <- results %>%
  select(variable_label, between_pct, within_pct) %>%
  pivot_longer(c(between_pct, within_pct),
               names_to = "component", values_to = "percentage") %>%
  mutate(component = factor(component,
                            levels = c("between_pct", "within_pct"),
                            labels = c("Between-Person", "Within-Person")))

p <- ggplot(plot_data,
            aes(x = reorder(variable_label, -percentage),
                y = percentage, fill = component)) +
  geom_col(position = "stack", width = 0.7, color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(
    values = c("Between-Person" = "#3498DB", "Within-Person" = "#E67E22"),
    name = "Variance Component") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     breaks = seq(0, 100, 20), expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Variance Decomposition: Between vs. Within Person",
    subtitle = "Estimation sample (n = 17). Components sum to 100% of total variance.",
    x = NULL, y = "Percentage of Total Variance") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold"))

ggsave("plot_variance_decomposition.png", p, width = 10, height = 6, dpi = 300)
cat("\nPlot opgeslagen: plot_variance_decomposition.png\n")