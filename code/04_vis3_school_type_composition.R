# ============================================================
# Visualisation 3: School Type Composition
# Main Q: How does geography shape educational inequality?
# Sub Q3: How does school type composition change as
#          remoteness increases?
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

main_levels <- c(
  "Primary School",
  "Secondary School",
  "Central/Community School",
  "Schools for Specific Purposes",
  "Infants School"
)

level_short <- c(
  "Primary School"                = "Primary",
  "Secondary School"              = "Secondary",
  "Central/Community School"      = "Central/Community",
  "Schools for Specific Purposes" = "Specific Purposes",
  "Infants School"                = "Infants"
)

df_heat <- df %>%
  filter(
    ASGS_remoteness %in% remoteness_order,
    Level_of_schooling %in% main_levels
  ) %>%
  count(ASGS_remoteness, Level_of_schooling) %>%
  group_by(ASGS_remoteness) %>%
  mutate(
    pct              = round(n / sum(n) * 100, 1),
    ASGS_remoteness  = factor(ASGS_remoteness, levels = remoteness_order),
    remote_short     = factor(
      remoteness_short[as.character(ASGS_remoteness)],
      levels = unname(remoteness_short)
    ),
    Level_of_schooling = factor(
      Level_of_schooling,
      levels = rev(main_levels)
    ),
    level_short = factor(
      level_short[as.character(Level_of_schooling)],
      levels = rev(unname(level_short))
    )
  ) %>%
  ungroup()

# Key stats for subtitle
cc_city   <- df_heat %>% filter(ASGS_remoteness == "Major Cities of Australia",  Level_of_schooling == "Central/Community School") %>% pull(pct)
cc_remote <- df_heat %>% filter(ASGS_remoteness == "Very Remote Australia",       Level_of_schooling == "Central/Community School") %>% pull(pct)
sec_city  <- df_heat %>% filter(ASGS_remoteness == "Major Cities of Australia",  Level_of_schooling == "Secondary School") %>% pull(pct)
sec_remote<- df_heat %>% filter(ASGS_remoteness == "Very Remote Australia",       Level_of_schooling == "Secondary School") %>% pull(pct)

p3 <- ggplot(
  df_heat,
  aes(
    x    = remote_short,
    y    = level_short,
    fill = pct
  )
) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(
    aes(
      label  = paste0(pct, "%"),
      colour = pct > 45
    ),
    size     = 3.8,
    fontface = "bold"
  ) +
  scale_fill_gradient(
    low    = "#EEF5FC",
    high   = "#1A5FA3",
    name   = "Share of schools (%)",
    limits = c(0, 80),
    breaks = seq(0, 80, 20),
    labels = paste0(seq(0, 80, 20), "%")
  ) +
  scale_colour_manual(
    values = c("FALSE" = "grey20", "TRUE" = "white"),
    guide  = "none"
  ) +
  labs(
    title    = "How does school type composition change as remoteness increases?",
    subtitle = paste0(
      "Central/Community schools rise from ", cc_city, "% in cities to ", cc_remote,
      "% in very remote areas — secondary schools fall from ", sec_city, "% to ", sec_remote, "%"
    ),
    x       = NULL,
    y       = NULL,
    caption = "Data source: NSW Department of Education, 2026"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey60", size = 8),
    panel.grid       = element_blank(),
    legend.position  = "right",
    legend.key.width = unit(0.8, "cm"),
    axis.text.x      = element_text(size = 10),
    axis.text.y      = element_text(size = 10)
  )

print(p3)

ggsave(
  "vis3_heatmap_composition.png",
  plot   = p3,
  width  = 11,
  height = 6,
  dpi    = 300
)
