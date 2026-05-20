library(tidyverse)
library(scales)

vars_to_decompose <- c("discovery_ratio", "gem_sessieduur_mins", 
                       "like_rate", "zoek_intensiteit")
var_labels <- c("Discovery Ratio", "Session Duration (mins)", 
                "Like-Rate", "Search Intensity")

calc_variance_decomp <- function(data, var_name) {
  clean_data <- data %>%
    filter(!is.na(!!sym(var_name))) %>%
    select(deelnemer, all_of(var_name))
  
  total_var <- var(clean_data[[var_name]], na.rm = TRUE)
  
  person_means <- clean_data %>%
  group_by(deelnemer) %>%
  summarise(mean_val = mean(!!sym(var_name), na.rm = TRUE), .groups = "drop")
  between_var <- var(person_means$mean_val, na.rm = TRUE)
  
  clean_data <- clean_data %>%
    group_by(deelnemer) %>%
    mutate(person_mean = mean(!!sym(var_name), na.rm = TRUE),
           deviation = !!sym(var_name) - person_mean) %>%
    ungroup()
  within_var <- var(clean_data$deviation, na.rm = TRUE)
  
  between_pct <- (between_var / total_var) * 100
  within_pct <- (within_var / total_var) * 100
  icc <- between_var / total_var
  
  tibble(
    variable = var_name,
    total_var = total_var,
    between_var = between_var,
    within_var = within_var,
    between_pct = between_pct,
    within_pct = within_pct,
    icc = icc,
    within_between_ratio = within_var / between_var
  )
}

results <- map_df(vars_to_decompose, ~calc_variance_decomp(panel, .x))
results <- results %>%
  mutate(variable_label = var_labels[match(variable, vars_to_decompose)])

variance_table <- results %>%
  select(
    Variable = variable_label,
    `Total Var` = total_var,
    `Between Var` = between_var,
    `Within Var` = within_var,
    `Between %` = between_pct,
    `Within %` = within_pct,
    ICC = icc,
    `Within/Between Ratio` = within_between_ratio
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 3)))

print(variance_table)

write_csv(variance_table, "table_variance_decomposition.csv")

plot_data <- results %>%
  select(variable_label, between_pct, within_pct) %>%
  pivot_longer(cols = c(between_pct, within_pct),
               names_to = "component",
               values_to = "percentage") %>%
  mutate(component = factor(component, 
                            levels = c("between_pct", "within_pct"),
                            labels = c("Between-Person", "Within-Person")))

p <- ggplot(plot_data, 
            aes(x = reorder(variable_label, -percentage), 
                y = percentage, 
                fill = component)) +
  geom_col(position = "stack", width = 0.7, color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 4.5) +
  scale_fill_manual(
    values = c("Between-Person" = "#3498DB", "Within-Person" = "#E67E22"),
    name = "Variance Component"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), 
                     breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Variance Decomposition: Between vs. Within Person",
    subtitle = "Fixed effects estimation relies exclusively on within-person variation",
    x = NULL,
    y = "Percentage of Total Variance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40", size = 11),
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold")
  )

ggsave("plot_variance_decomposition.png", p, width = 10, height = 6, dpi = 300)
cat("✓ Plot saved: plot_variance_decomposition.png\n")