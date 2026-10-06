# ============================================================
# Visualisation 4: Selective School Access
# Main Q: How does geography shape educational inequality?
# Sub Q4: Are selective schools geographically accessible
#          to students outside major cities?
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

df_sel <- df %>%
  filter(
    ASGS_remoteness %in% remoteness_order,
    Selective_school %in% c("Fully Selective", "Partially Selective")
  ) %>%
  count(ASGS_remoteness, Selective_school) %>%
  mutate(
    ASGS_remoteness  = factor(ASGS_remoteness, levels = remoteness_order),
    remote_short     = factor(
      remoteness_short[as.character(ASGS_remoteness)],
      levels = unname(remoteness_short)
    ),
    Selective_school = factor(
      Selective_school,
      levels = c("Fully Selective", "Partially Selective")
    )
  )

sel_totals <- df_sel %>%
  group_by(remote_short, ASGS_remoteness) %>%
  summarise(total = sum(n), .groups = "drop")

all_zones <- tibble(
  remote_short    = factor(unname(remoteness_short), levels = unname(remoteness_short)),
  ASGS_remoteness = factor(remoteness_order, levels = remoteness_order)
)

sel_totals_full <- all_zones %>%
  left_join(sel_totals, by = c("remote_short", "ASGS_remoteness")) %>%
  mutate(total = replace_na(total, 0))

# Key stats for subtitle
n_metro_sel  <- sel_totals_full %>% filter(ASGS_remoteness == "Major Cities of Australia") %>% pull(total)
n_total_sel  <- sum(sel_totals_full$total)
pct_metro    <- round(n_metro_sel / n_total_sel * 100, 0)

p4 <- ggplot(
  sel_totals_full,
  aes(
    x    = remote_short,
    y    = total,
    fill = ASGS_remoteness
  )
) +
  geom_col(width = 0.6, show.legend = FALSE) +
  # Value labels on bars
  geom_text(
    aes(
      label = ifelse(total > 0, total, "None"),
      y     = ifelse(total > 0, total + 0.8, 0.8)
    ),
    size   = 4,
    colour = "grey30",
    fontface = "bold"
  ) +
  # Annotate zero bars
  annotate(
    "text",
    x      = c(4, 5),
    y      = 1.5,
    label  = "No selective\nschools",
    size   = 3,
    colour = "grey60",
    lineheight = 1.3
  ) +
  scale_fill_manual(values = remoteness_colours) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Are selective schools geographically accessible to students outside major cities?",
    subtitle = paste0(
      pct_metro, "% of all selective schools are in major cities — ",
      "Remote and Very Remote NSW have none"
    ),
    x       = NULL,
    y       = "Number of selective schools",
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

print(p4)

ggsave(
  "vis4_selective_access.png",
  plot   = p4,
  width  = 11,
  height = 7,
  dpi    = 300
)
