# ============================================================
# COEFFICIENT PLOTS voor Results sectie (Figures 4 en 5)
# ============================================================
# Dit script maakt coefficient plots met 95% confidence intervals
# voor alle 4 modellen per dependent variable.
#
# Voordat je dit script draait, zorg dat je de modellen hebt gedraaid
# zoals in je originele analyse script.

library(fixest)
library(dplyr)
library(ggplot2)
library(broom)

# Verondersteld dat je modellen al geladen zijn in je environment
# m1_sess, m2_sess, m3_sess, m4_sess (Session Duration)
# m5_like, m6_like, m7_like, m8_like (Like-Rate)

# Als je de modellen opnieuw moet draaien, draai eerst je analyse script

# ============================================================
# FIGURE 4: Session Duration Coefficients
# ============================================================

# Extraheer coefficients en standard errors voor Discovery Ratio
coef_data_sess <- data.frame(
  Model = c("Model 1\n(OLS Baseline)", 
            "Model 2\n(OLS + Moderation)", 
            "Model 3\n(TWFE Baseline)", 
            "Model 4\n(TWFE + Moderation)"),
  Estimate = c(
    coef(m1_sess)["discovery_ratio"],
    coef(m2_sess)["discovery_ratio"],
    coef(m3_sess)["discovery_ratio"],
    coef(m4_sess)["discovery_ratio"]
  ),
  SE = c(
    se(m1_sess)["discovery_ratio"],
    se(m2_sess)["discovery_ratio"],
    se(m3_sess, vcov = "DK")["discovery_ratio"],
    se(m4_sess, vcov = "DK")["discovery_ratio"]
  )
) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE,
    Model = factor(Model, levels = c(
      "Model 1\n(OLS Baseline)",
      "Model 2\n(OLS + Moderation)",
      "Model 3\n(TWFE Baseline)",
      "Model 4\n(TWFE + Moderation)"
    ))
  )

# Plot Figure 4
fig4 <- ggplot(coef_data_sess, aes(x = Model, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "#2C3E50") +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    linewidth = 0.8,
    color = "#2C3E50"
  ) +
  labs(
    title = "Figure 4. Discovery Ratio Effect on Session Duration Across Model Specifications",
    x = "",
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Save Figure 4
ggsave(
  "Figure_4_Session_Duration_Coefficients.png",
  plot = fig4,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

print(fig4)

# ============================================================
# FIGURE 5: Like-Rate Coefficients
# ============================================================

# Extraheer coefficients en standard errors voor Discovery Ratio
coef_data_like <- data.frame(
  Model = c("Model 5\n(OLS Baseline)", 
            "Model 6\n(OLS + Moderation)", 
            "Model 7\n(TWFE Baseline)", 
            "Model 8\n(TWFE + Moderation)"),
  Estimate = c(
    coef(m5_like)["discovery_ratio"],
    coef(m6_like)["discovery_ratio"],
    coef(m7_like)["discovery_ratio"],
    coef(m8_like)["discovery_ratio"]
  ),
  SE = c(
    se(m5_like)["discovery_ratio"],
    se(m6_like)["discovery_ratio"],
    se(m7_like, vcov = "DK")["discovery_ratio"],
    se(m8_like, vcov = "DK")["discovery_ratio"]
  )
) %>%
  mutate(
    CI_lower = Estimate - 1.96 * SE,
    CI_upper = Estimate + 1.96 * SE,
    Model = factor(Model, levels = c(
      "Model 5\n(OLS Baseline)",
      "Model 6\n(OLS + Moderation)",
      "Model 7\n(TWFE Baseline)",
      "Model 8\n(TWFE + Moderation)"
    ))
  )

# Plot Figure 5
fig5 <- ggplot(coef_data_like, aes(x = Model, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, color = "#2C3E50") +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.2,
    linewidth = 0.8,
    color = "#2C3E50"
  ) +
  labs(
    title = "Figure 5. Discovery Ratio Effect on Like-Rate Across Model Specifications",
    x = "",
    y = "Coefficient Estimate (95% CI)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Save Figure 5
ggsave(
  "Figure_5_Like_Rate_Coefficients.png",
  plot = fig5,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

print(fig5)

# ============================================================
# Print summary voor verificatie
# ============================================================

cat("\n══ Figure 4 Data (Session Duration) ═══════════════════\n")
print(coef_data_sess)

cat("\n══ Figure 5 Data (Like-Rate) ══════════════════════════\n")
print(coef_data_like)

cat("\n✓ Figures saved as PNG files in working directory\n")
cat("  - Figure_4_Session_Duration_Coefficients.png\n")
cat("  - Figure_5_Like_Rate_Coefficients.png\n")