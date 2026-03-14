library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(here)
library(ggtext)
library(ggraph)
library(igraph)
library(circlize)
library(showtext)
library(svglite)


pairwise_comparisons <- read_tt_csv(
  local_name = "tt_2026_w10_pairwise_comparisons.csv",
  remote_url = "https://raw.githubusercontent.com/adamkucharski/CAPphrase/main/data/pairwise_comparisons.csv"
) |>
  clean_names()

pairwise_outcomes <- pairwise_comparisons |>
  mutate(
    loser = if_else(selected == term1, term2, term1)
  )

pairwise_strength <- pairwise_outcomes |>
  count(selected, loser, sort = TRUE) |>
  rename(winner = selected) |>
  slice_max(n, n = 25)

arc_nodes <- term_summary |>
  arrange(median_probability) |>
  mutate(
    name = as.character(term),
    node_label = paste0(as.character(term), " (", round(median_probability), "%)")
  )

g_arc <- graph_from_data_frame(
  pairwise_strength |> rename(from = loser, to = winner),
  vertices = arc_nodes |> select(name, median_probability, node_label)
)

arc_layout <- create_layout(g_arc, layout = "linear")
arc_layout$y <- arc_layout$x
arc_layout$x <- 0

p_arc <- ggraph(arc_layout) +
  geom_edge_arc(
    aes(width = n, colour = n),
    alpha = 0.72,
    fold = TRUE
  ) +
  geom_node_point(
    aes(fill = median_probability),
    size = 7, shape = 21, colour = "white", stroke = 1.5
  ) +
  geom_node_text(
    aes(label = node_label),
    hjust = 1, vjust = 0.5,
    size = 3, colour = "grey25",
    nudge_x = -0.15
  ) +
  scale_edge_colour_gradient(
    low = "#1d3557", high = "#1d3557",
    name = "Times chosen",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "lines"),
      barheight = unit(0.5, "lines"),
      ticks = FALSE
    )
  ) +
  scale_edge_width(range = c(0.4, 2.8), guide = "none") +
  scale_fill_gradient(
    low = "#006e90", high = "#f18f01",
    labels = label_percent(scale = 1),
    name = "Median probability",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(6, "lines"),
      barheight = unit(0.5, "lines"),
      ticks = FALSE
    )
  ) +
  labs(
    title = "How People Rank Probability Phrases Head-to-Head",
    subtitle = paste0(
      "Phrases ordered bottom \u2192 top by median probability estimate.\n",
      "Arcs run from the less-probable phrase to the more-probable one \u2014 thicker arcs were chosen more often."
    ),
    caption = "Source: CAPphrase / TidyTuesday 2026 W10 | Viz: @deepali"
  ) +
  coord_cartesian(clip = "off") +
  theme_graph(base_family = "inter") +
  theme(
    text                = element_text(),
    legend.position     = "bottom",
    legend.box          = "horizontal",
    legend.spacing.x    = unit(1.5, "cm"),
    legend.title        = element_text(size = 8.5, face = "bold", colour = "#101820"),
    legend.text         = element_text(size = 7.5, colour = "grey40"),
    legend.margin       = margin(t = 8),
    plot.margin         = margin(t = 15, r = 20, b = 20, l = 120),
    plot.title          = element_text(size = 16, face = "bold", colour = "#101820"),
    plot.subtitle       = element_text(size = 9.5, colour = "grey50", lineheight = 1.45),
    plot.caption        = element_text(size = 7.5, colour = "grey60", hjust = 1, )
  )

p_arc

width_px <- 700
height_px <- 1100
dpi <- 96

showtext_auto(enable = FALSE)
svglite("p_arc.svg", width = width_px / dpi, height = height_px / dpi)
print(p_arc)
dev.off()
