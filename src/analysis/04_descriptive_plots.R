library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)

panel_all <- read_csv("gen/analysis/weekly_panel_all.csv",
                      show_col_types = FALSE)

variabele_labels <- c(
  discovery_ratio      = "Discovery Ratio\n(aandeel nieuwe creators)",
  gem_sessieduur_mins  = "Gem. Sessieduur\n(minuten)",
  like_rate            = "Like-Rate\n(likes / bekeken video's)",
  zoek_intensiteit     = "Zoekintensiteit\n(aantal zoekopdrachten)"
)

panel_long <- panel_all %>%
  select(deelnemer, week,
         discovery_ratio, gem_sessieduur_mins,
         like_rate, zoek_intensiteit) %>%
  pivot_longer(
    cols      = c(discovery_ratio, gem_sessieduur_mins,
                  like_rate, zoek_intensiteit),
    names_to  = "variabele",
    values_to = "waarde"
  ) %>%
  mutate(
        variabele = factor(variabele,
                       levels = names(variabele_labels),
                       labels = variabele_labels)
  )


plot_tijdlijn <- ggplot(panel_long,
                        aes(x = week, y = waarde,
                            colour = deelnemer, group = deelnemer)) +
  geom_line(alpha = 0.7, linewidth = 0.6) +
  geom_point(size = 0.8, alpha = 0.5) +
  facet_wrap(~variabele, scales = "free_y", ncol = 2) +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b %Y") +
  labs(
    title    = "Verloop van de onderzoeksvariabelen per deelnemer",
    subtitle = "Elke lijn = één deelnemer | Wekelijkse waarden",
    x        = NULL,
    y        = "Waarde",
    colour   = "Deelnemer"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    legend.position  = "bottom",
    legend.key.width = unit(1.5, "cm"),
    panel.grid.minor = element_blank()
  )

ggsave("gen/analysis/plot1_tijdlijn.png",
       plot   = plot_tijdlijn,
       width  = 14, height = 9, dpi = 150)

print(plot_tijdlijn)

plot_verdeling <- ggplot(panel_long,
                         aes(x = variabele, y = waarde)) +
  geom_boxplot(outlier.shape = NA,           # uitbijters via jitter, niet dubbel
               fill = "#D0E8F2", colour = "#2C7BB6",
               width = 0.5, linewidth = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.25,
              size = 0.9, colour = "#2C7BB6") +
  facet_wrap(~variabele, scales = "free", ncol = 2) +
  labs(
    title    = "Verdeling van de onderzoeksvariabelen",
    subtitle = "Alle deelnemers × alle weken | Punten buiten de whiskers zijn uitbijters",
    x        = NULL,
    y        = "Waarde"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_blank(),   
    axis.ticks.x     = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("gen/analysis/plot2_verdeling.png",
       plot   = plot_verdeling,
       width  = 12, height = 8, dpi = 150)

print(plot_verdeling)

panel_all %>%
  select(deelnemer, week,
         discovery_ratio, gem_sessieduur_mins,
         like_rate, zoek_intensiteit) %>%
  pivot_longer(-c(deelnemer, week),
               names_to = "variabele", values_to = "waarde") %>%
  group_by(variabele) %>%
  mutate(p99 = quantile(waarde, 0.99, na.rm = TRUE)) %>%
  filter(waarde > p99) %>%
  select(variabele, deelnemer, week, waarde, p99) %>%
  arrange(variabele, desc(waarde)) %>%
  print(n = 50)