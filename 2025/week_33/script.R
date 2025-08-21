library(tidyverse)
library(showtext)
library(janitor)
library(scales)
library(glue)
library(ggtext)
library(ggpattern)


font_add_google("Overpass", "arvo")
showtext_auto()


grad_start <- c(
  "Munro" = "#2E8B57", 
  "Munro Top" = "#4B0082"
) 
grad_end <- c(
  "Munro" = "#98FB98", 
  "Munro Top" = "#DA70D6"
) 


triangles <- df %>%
  filter(!is.na(category)) %>%
  count(year, category, name = "n") %>%
  mutate(
    category = factor(category, levels = c("Munro", "Munro Top")),
    x_num = as.numeric(category),
    id = paste(year, category, sep = "_"),
    x_poly = map(x_num, ~ c(.x - 0.42, .x, .x + 0.42)), # base L, peak, base R
    y_poly = map(n, ~ c(0, .x, 0))
  ) %>%
  unnest(c(x_poly, y_poly))


labels <- triangles %>%
  group_by(id, year, category) %>%
  summarise(
    x_lab = x_poly[2],
    y_lab = 0.20 * y_poly[2], # visually centered
    n = first(y_poly[2]),
    .groups = "drop"
  )


title_text <- "Scottish Munros"

subtitle_text <- glue(
  "A <span style='font-weight:700;color:{grad_end['Munro']};'>Munro</span> is a Scottish mountain over 3,000 ft; a \
<span style='font-weight:700;color:{grad_end['Munro Top']};'>Munro Top</span> is a subsidiary summit over 3,000 ft \
but not a distinct mountain. The most famous Munro is Ben Nevis. In 1891, Sir Hugh Munro produced the first list. \
This chart shows the number classified as Munro and Munro Top across survey years."
)

caption <- "Data: hills-database.co.uk.| Graphic: Deepali Kank"



ggplot(triangles, aes(x = x_poly, y = y_poly, group = id)) +
  ggpattern::geom_polygon_pattern(
    aes(pattern_fill = category, pattern_fill2 = category),
    pattern = "gradient",
    pattern_angle = 90, # vertical gradient: darker base -> lighter peak
    pattern_alpha = 1,
    color = NA,
    show.legend = FALSE
  ) +
  
  geom_text(
    data = labels,
    aes(x = x_lab, y = y_lab, label = n),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 5,
    color = "#f8f9fa"
  ) +
  scale_pattern_fill_manual(values = grad_start) +
  scale_pattern_fill2_manual(values = grad_end) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  facet_wrap(~year, strip.position = "top") +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption,
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 10, base_family = "arvo") +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill = "black"),
    strip.text = element_text(size = 13, face = "bold", color = "#f8f9fa"),
    strip.background = element_blank(),
    plot.title = element_textbox_simple(margin = margin(b = 10), size = 40, face = "bold", color = "#f8f9fa"),
    plot.subtitle = element_textbox_simple(margin = margin(b = 15), size = 20, lineheight = 0.65, color = "#f8f9fa"),
    plot.caption = element_text(margin = margin(t = 10), size = 15, hjust = 0.5, color = "#f8f9fa"),
    plot.margin = margin(12, 14, 12, 14),
    panel.spacing.x = unit(12, "pt"),
    panel.spacing.y = unit(12, "pt")
  )
