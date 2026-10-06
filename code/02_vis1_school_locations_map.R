# ============================================================
# Visualisation 1: Geographic Distribution
# Main Q: How does geography shape educational inequality?
# Sub Q1: Where are NSW public schools located and how are
#          they distributed across the state?
# ============================================================

library(tidyverse)
library(scales)

# install.packages("ozmaps")  # run once if not installed
# install.packages("sf")  # run once if not installed
library(ozmaps)
library(sf)


df <- read_csv("NSW_schools_cleaned.csv")


main_levels <- c(
  "Primary School",
  "Secondary School",
  "Central/Community School",
  "Schools for Specific Purposes",
  "Infants School"
)

df_map <- df %>%
  filter(
    Level_of_schooling %in% main_levels,
    !is.na(Latitude),
    !is.na(Longitude),
    !is.na(latest_year_enrolment_FTE)
  ) %>%
  mutate(
    Level_of_schooling = factor(Level_of_schooling, levels = main_levels)
  ) %>%
  # Sort so smaller dots plot on top
  arrange(desc(latest_year_enrolment_FTE))


nsw_boundary <- ozmap_states %>%
  filter(NAME == "New South Wales")

level_colours <- c(
  "Primary School"                = "#378ADD",
  "Secondary School"              = "#1D9E75",
  "Central/Community School"      = "#BA7517",
  "Schools for Specific Purposes" = "#D4537E",
  "Infants School"                = "#888780"
)

n_total    <- nrow(df_map)
n_metro    <- sum(df_map$ASGS_remoteness == "Major Cities of Australia")
n_remote   <- sum(df_map$ASGS_remoteness %in% c("Remote Australia", "Very Remote Australia"))
pct_metro  <- round(n_metro / n_total * 100, 0)

# ============================================================
# VISUALISATION 1: Map
# ============================================================
p1 <- ggplot() +
  # NSW state boundary
  geom_sf(
    data      = nsw_boundary,
    fill      = "#F5F5F0",
    colour    = "grey60",
    linewidth = 0.4
  ) +
  # School dots
  geom_point(
    data  = df_map,
    aes(
      x      = Longitude,
      y      = Latitude,
      colour = Level_of_schooling,
      size   = latest_year_enrolment_FTE
    ),
    alpha = 0.6
  ) +
  # Size scale
  scale_size_continuous(
    name   = "Enrolment (FTE)",
    range  = c(0.5, 6),
    breaks = c(100, 500, 1000, 2000),
    labels = comma
  ) +
  # Colour scale
  scale_colour_manual(
    values = level_colours,
    name   = "School type"
  ) +
  # Map bounds — mainland NSW + small buffer
  coord_sf(
    xlim = c(140.9, 154),
    ylim = c(-37.5, -27.8),
    expand = FALSE
  ) +
  # Labels
  labs(
    title    = "Where are NSW public schools located?",
    subtitle = paste0(
      pct_metro, "% of schools cluster along the coastal corridor — ",
      n_remote, " schools serve the vast inland and remote west"
    ),
    x        = "Longitude",
    y        = "Latitude",
    caption  = "Data source: NSW Department of Education, 2026"
  ) +
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10, margin = margin(b = 10)),
    plot.caption     = element_text(colour = "grey60", size = 8),
    legend.position  = "right",
    legend.text      = element_text(size = 9),
    legend.title     = element_text(size = 9, face = "bold"),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text        = element_text(size = 8, colour = "grey50")
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 3, alpha = 1)),
    size   = guide_legend(override.aes = list(alpha = 0.7))
  )

print(p1)

ggsave(
  "vis1_geographic_distribution.png",
  plot   = p1,
  width  = 12,
  height = 9,
  dpi    = 300
)
