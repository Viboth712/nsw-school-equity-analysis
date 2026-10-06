# ============================================================
# Visualisation 5: ICSEA vs Enrolment Regression
# Main Q: How does geography shape educational inequality?
# Sub Q5: Does socioeconomic disadvantage predict school size?
# ============================================================

library(tidyverse)
library(scales)

df <- read_csv("NSW_schools_cleaned.csv")

remoteness_order <- c(
  "Major Cities of Australia",
  "Inner Regional Australia",
  "Outer Regional Australia",
  "Remote Australia",
  "Very Remote Australia"
)

remoteness_short <- c(
  "Major Cities of Australia" = "Major Cities",
  "Inner Regional Australia"  = "Inner Regional",
  "Outer Regional Australia"  = "Outer Regional",
  "Remote Australia"          = "Remote",
  "Very Remote Australia"     = "Very Remote"
)

remoteness_colours <- c(
  "Major Cities of Australia" = "#378ADD",
  "Inner Regional Australia"  = "#1D9E75",
  "Outer Regional Australia"  = "#BA7517",
  "Remote Australia"          = "#D4537E",
  "Very Remote Australia"     = "#E24B4A"
)

df_reg <- df %>%
  filter(
    !is.na(ICSEA_value),
    !is.na(latest_year_enrolment_FTE),
    ASGS_remoteness %in% remoteness_order
  ) %>%
  mutate(
    ASGS_remoteness = factor(ASGS_remoteness, levels = remoteness_order),
    remote_short    = remoteness_short[as.character(ASGS_remoteness)]
  )

# Run linear regression
model <- lm(latest_year_enrolment_FTE ~ ICSEA_value, data = df_reg)
summary_model <- summary(model)

# Extract key stats
r_squared <- round(summary_model$r.squared, 3)
slope     <- round(coef(model)["ICSEA_value"], 2)
intercept <- round(coef(model)["(Intercept)"], 1)
pearson_r <- round(sqrt(r_squared) * sign(slope), 2)

# Predicted vs actual for annotation
df_reg <- df_reg %>%
  mutate(
    predicted  = predict(model, newdata = .),
    residual   = latest_year_enrolment_FTE - predicted,
    over_under = ifelse(residual > 0, "Above predicted", "Below predicted")
  )

cat(paste0("Regression: enrolment = ", slope, " x ICSEA + (", intercept, ")\n"))
cat(paste0("Pearson r = ", pearson_r, "\n"))
cat(paste0("R-squared = ", r_squared, "\n"))

p5 <- ggplot(
  df_reg,
  aes(
    x      = ICSEA_value,
    y      = latest_year_enrolment_FTE,
    colour = ASGS_remoteness
  )
) +
  # Scatter points
  geom_point(alpha = 0.4, size = 1.5) +
  # Regression line with confidence interval
  geom_smooth(
    method    = "lm",
    se        = TRUE,
    colour    = "grey20",
    fill      = "grey80",
    linewidth = 1,
    inherit.aes = FALSE,
    aes(x = ICSEA_value, y = latest_year_enrolment_FTE)
  ) +
  # National average ICSEA reference line
  geom_vline(
    xintercept = 1000,
    colour     = "grey50",
    linetype   = "dashed",
    linewidth  = 0.5
  ) +
  # Regression equation annotation
  annotate(
    "text",
    x          = 620,
    y          = 2300,
    label      = paste0(
      "y = ", slope, "x + (", intercept, ")\n",
      "Pearson r = ", pearson_r, "\n",
      "R² = ", r_squared
    ),
    size       = 3.2,
    colour     = "grey20",
    hjust      = 0,
    lineheight = 1.5,
    fontface   = "italic"
  ) +
  # National average label
  annotate(
    "text",
    x      = 1008,
    y      = 2400,
    label  = "National\naverage\n(1,000)",
    size   = 2.8,
    colour = "grey50",
    hjust  = 0,
    lineheight = 1.3
  ) +
  scale_colour_manual(
    values = remoteness_colours,
    name   = "Remoteness"
  ) +
  scale_x_continuous(
    breaks = seq(600, 1300, 100),
    labels = comma
  ) +
  scale_y_continuous(
    labels = comma,
    breaks = seq(0, 2500, 500)
  ) +
  labs(
    title    = "Does socioeconomic disadvantage predict school size?",
    subtitle = paste0(
      "Yes — every 100-point increase in ICSEA predicts ", round(slope * 100),
      " more students. Remote schools cluster bottom-left: disadvantaged and small"
    ),
    x       = "ICSEA value (higher = less disadvantaged)",
    y       = "Student enrolment (FTE)",
    caption = "Data source: NSW Department of Education, 2026. ICSEA: Index of Community Socio-Educational Advantage (ACARA)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey60", size = 8),
    panel.grid.minor = element_blank(),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 9, face = "bold")
  ) +
  guides(
    colour = guide_legend(override.aes = list(alpha = 1, size = 3))
  )

print(p5)

ggsave(
  "vis5_regression_icsea.png",
  plot   = p5,
  width  = 12,
  height = 7,
  dpi    = 300
)
