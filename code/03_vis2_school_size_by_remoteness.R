# ============================================================
# Visualisation 2: School Size by Remoteness
# Main Q: How does geography shape educational inequality?
# Sub Q2: How does school size differ across remoteness categories?
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

df_box <- df %>%
  filter(
    ASGS_remoteness %in% remoteness_order,
    !is.na(latest_year_enrolment_FTE)
  ) %>%
  mutate(
    ASGS_remoteness = factor(ASGS_remoteness, levels = remoteness_order),
    remote_short    = factor(
      remoteness_short[as.character(ASGS_remoteness)],
      levels = unname(remoteness_short)
    )
  )

# Summary stats for subtitle
mean_city   <- df_box %>% filter(ASGS_remoteness == "Major Cities of Australia") %>% pull(latest_year_enrolment_FTE) %>% mean() %>% round()
mean_remote <- df_box %>% filter(ASGS_remoteness == "Very Remote Australia") %>% pull(latest_year_enrolment_FTE) %>% mean() %>% round()

# Stats per group for annotation
stats <- df_box %>%
  group_by(remote_short, ASGS_remoteness) %>%
  summarise(
    median_enr = round(median(latest_year_enrolment_FTE), 0),
    n          = n(),
    .groups    = "drop"
  )

p2 <- ggplot(
  df_box,
  aes(
    x    = remote_short,
    y    = latest_year_enrolment_FTE,
    fill = ASGS_remoteness
  )
) +
  geom_boxplot(
    outlier.shape  = 16,
    outlier.size   = 1,
    outlier.alpha  = 0.3,
    outlier.colour = "grey50",
    width          = 0.55,
    show.legend    = FALSE
  ) +
  # Median labels
  geom_text(
    data = stats,
    aes(
      x     = remote_short,
      y     = median_enr,
      label = paste0("Median:\n", comma(median_enr))
    ),
    hjust       = -0.15,
    size        = 3,
    colour      = "grey30",
    inherit.aes = FALSE
  ) +
  # Sample size labels
  geom_text(
    data = stats,
    aes(
      x     = remote_short,
      y     = -50,
      label = paste0("n = ", comma(n))
    ),
    size        = 3,
    colour      = "grey50",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = remoteness_colours) +
  scale_y_continuous(
    labels = comma,
    limits = c(-80, 2500),
    breaks = seq(0, 2500, 500)
  ) +
  labs(
    title    = "How does school size differ across remoteness categories?",
    subtitle = paste0(
      "Major city schools average ", comma(mean_city),
      " students — nearly 10x more than very remote schools (", comma(mean_remote), ")"
    ),
    x       = NULL,
    y       = "Student enrolment (FTE)",
    caption = "Data source: NSW Department of Education, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption       = element_text(colour = "grey60", size = 8),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 10)
  )

print(p2)

ggsave(
  "vis2_boxplot_remoteness.png",
  plot   = p2,
  width  = 12,
  height = 7,
  dpi    = 300
)
