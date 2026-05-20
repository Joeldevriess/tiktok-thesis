library(fixest)
library(dplyr)
library(ggplot2)

# FIGURE 6: Session Duration Coefficients
coef_data_sess <- data.frame(
  Model = c("Model 1\nOLS", 
            "Model 2\nTime FE", 
            "Model 3\nTWFE"),
  Estimate = c(
    coef(m1_sess)["discovery_ratio"],
    coef(m2_sess)["discovery_ratio"],
    coef(m3_sess)["discovery_ratio"]
  ),
  SE = c(
    se(m1_sess)["discovery_ratio"],
    se(m2_sess)["discovery_ratio"],
    se(m3_sess, vcov = "DK")["discovery_ratio"]
  )
) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE,
    Model = factor(Model, levels = c(
      "Model 1\nOLS",
      "Model 2\nTime FE",
      "Model 3\nTWFE"
    ))
  )

fig_sess <- ggplot(coef_data_sess, aes(x = Model, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.15,
    linewidth = 1,
    color = "#2C3E50"
  ) +
  labs(
    title = "Discovery Ratio Effect on Session Duration",
    subtitle = "Progressive robustness checks",
    x = "",
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  "gen/figures/Figure_Session_Duration_Coefficients.png",
  plot = fig_sess,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

print(fig_sess)

# FIGURE 7: Like-Rate Coefficients

coef_data_like <- data.frame(
  Model = c("Model 4\nOLS", 
            "Model 5\nTime FE", 
            "Model 6\nTWFE"),
  Estimate = c(
    coef(m4_like)["discovery_ratio"],
    coef(m5_like)["discovery_ratio"],
    coef(m6_like)["discovery_ratio"]
  ),
  SE = c(
    se(m4_like)["discovery_ratio"],
    se(m5_like)["discovery_ratio"],
    se(m6_like, vcov = "DK")["discovery_ratio"]
  )
) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE,
    Model = factor(Model, levels = c(
      "Model 4\nOLS",
      "Model 5\nTime FE",
      "Model 6\nTWFE"
    ))
  )

fig_like <- ggplot(coef_data_like, aes(x = Model, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(size = 4, color = "#2C3E50") +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.15,
    linewidth = 1,
    color = "#2C3E50"
  ) +
  labs(
    title = "Discovery Ratio Effect on Like-Rate",
    subtitle = "Progressive robustness checks",
    x = "",
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 20, 10, 10)
  )

ggsave(
  "gen/figures/Figure_Like_Rate_Coefficients.png",
  plot = fig_like,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

print(fig_like)