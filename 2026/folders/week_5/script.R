
# Libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(here)
library(ggtext)
library(ggiraph)
library(showtext)

font_add_google("Outfit", "outfit")
font_add_google("DM Sans", "dm_sans")
showtext_auto()

# Load data ----
df <- tidytuesdayR::tt_load(2026, week = 5)$edible_plants

df <- df |>
  separate_wider_delim(
    soil,
    delim = ",",
    names = c("soil_primary", "soil_rest"),
    too_few = "align_start",
    too_many = "merge"
  ) |>
  mutate(
    soil_primary = str_trim(soil_primary),
    soil_primary = case_when(
      str_detect(str_to_lower(soil_primary), "loam") ~ "Loamy",
      str_detect(str_to_lower(soil_primary), "sand") ~ "Sandy",
      str_detect(str_to_lower(soil_primary), "chalk") ~ "Chalky",
      str_detect(str_to_lower(soil_primary), "clay") ~ "Clay",
      str_detect(str_to_lower(soil_primary), "well.drained") ~ "Well-drained",
      str_detect(str_to_lower(soil_primary), "all|most") ~ "Any",
      str_detect(str_to_lower(soil_primary), "rich|moist") ~ "Rich moist",
      .default = soil_primary
    )
  )

df |>
  count(soil_primary, sort = TRUE) |>
  print(n = Inf)

df |>
  count(nutrients, sort = TRUE)

df |>
  count(temperature_class, sort = TRUE)

# Soil bar fill colors (muted, earthy)
soil_colors <- c(
  "Loamy" = "#C4A265",
  "Sandy" = "#D9C5A0",
  "Chalky" = "#B8A99A",
  "Clay" = "#8B5E3C",
  "Well-drained" = "#6B4F3A",
  "Any" = "#7A6B5D",
  "Rich moist" = "#3E2E1E"
)

# Dot colors — distinct per soil, contrasting against the bar fills
dot_colors <- c(
  "Loamy" = "#4A7C3F",
  "Sandy" = "#6B8F5E",
  "Chalky" = "#7B6B8D",
  "Clay" = "#D4A853",
  "Well-drained" = "#A8C686",
  "Any" = "#C9B458",
  "Rich moist" = "#8FBC6B"
)

# Prep plot data: jitter x/y positions for dots within each soil strip
set.seed(42)
df_plot <- df |>
  filter(!is.na(soil_primary)) |>
  mutate(
    soil_primary = fct_reorder(soil_primary, soil_primary, length, .desc = TRUE)
  ) |>
  group_by(soil_primary) |>
  mutate(
    x = runif(n(), 0.05, 0.95),
    y = runif(n(), 0.15, 0.85)
  ) |>
  ungroup()

# Plot ----
bg_color <- "#262626"

# Build a rect summary so geom_rect draws once per panel (not per row)
df_rects <- df_plot |>
  distinct(soil_primary) |>
  mutate(fill_color = soil_colors[as.character(soil_primary)])

p <- ggplot(df_plot, aes(x = x, y = y)) +
  geom_rect(
    data = df_rects,
    aes(fill = I(fill_color)),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.9,
    inherit.aes = FALSE
  ) +
  geom_point_interactive(
    aes(tooltip = common_name, data_id = common_name, color = soil_primary),
    size = 4,
    alpha = 0.3,
    stroke = 0.8,
    shape = 21,
    fill = "white"
  ) +
  geom_point_interactive(
    aes(tooltip = common_name, data_id = common_name, color = soil_primary),
    size = 2.5,
    alpha = 0.85,
    stroke = 0,
    shape = 16
  ) +
  scale_color_manual(values = dot_colors) +
  facet_wrap(~soil_primary, ncol = 1, strip.position = "left") +
  labs(
    title = "What Soil do edible plants prefer?",
    subtitle = "Each dot represents one edible plant. Hover to see the plant name.",
    caption = "Source: TidyTuesday 2026 Week 5 | Graphic: @deepalikank"
  ) +
  theme_void(base_family = "dm_sans") +
  theme(
    plot.background = element_rect(fill = bg_color, color = NA),
    strip.text.y.left = element_text(
      angle = 0,
      hjust = 1,
      size = 14,
      face = "bold",
      color = "grey80",
      family = "outfit",
      margin = margin(r = 25)
    ),
    panel.spacing = unit(4, "pt"),
    plot.title = element_text(
      size = 26,
      face = "bold",
      color = "#d7d7d7",
      family = "outfit",
      margin = margin(b = 6)
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "grey60",
      family = "dm_sans",
      margin = margin(b = 22)
    ),
    plot.caption = element_text(
      size = 10,
      color = "#f6f6f6",
      family = "dm_sans",
      margin = margin(t = 18),
      hjust = 1
    ),
    plot.margin = margin(30, 30, 25, 30),
    legend.position = "none"
  )

girafe(
  ggobj = p,
  width_svg = 10,
  height_svg = 8,
  options = list(
    opts_hover(css = "fill:white;stroke:white;r:5pt;opacity:1;"),
    opts_tooltip(
      css = glue(
        "background:{bg_color};color:white;padding:8px 14px;",
        "border-radius:6px;font-size:13px;font-family:'DM Sans',sans-serif;",
        "border:1px solid rgba(255,255,255,0.15);",
        "box-shadow:0 4px 12px rgba(0,0,0,0.4);"
      )
    ),
    opts_hover_inv(css = "opacity:0.3;"),
    opts_sizing(rescale = TRUE, width = 1)
  )
)
