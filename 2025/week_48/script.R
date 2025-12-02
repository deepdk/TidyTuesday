# Load necessary libraries
library(tidyverse)
library(zoo)
library(ggtext)
library(glue)
library(grid)
library(showtext)

# palette
font_add_google("Open Sans", "opensans")
showtext_auto()

record_cols <- c(
  "FALSE" = "#087e8b", # non-record years
  "TRUE"  = "#ff5a5f" # record-hot summers
)

# rich subtitle text
subtitle_text <- glue(
  "Folklore says a faster Boeoegg blast means a finer summer, ",
  "but the pattern is messy: ",
  "<span style='color:{record_cols['TRUE']};'>years above 19°C</span> ",
  "and <span style='color:{record_cols['FALSE']};'>years at or below 19°C</span> ",
  "both show up after short and long burn durations, so the Boeoegg is at best a very rough guide."
)

caption <- "Data: #TidyTuesday Week 48 2025.| Graphic: Deepali Kank"


ggplot(data, aes(x = duration, y = tre200m0)) +
  geom_point(aes(color = record), alpha = 0.4, size = 0) +
  geom_smooth(method = "lm", color = "black", se = FALSE, size = 0.7, linetype = "dotted") +
  geom_label(
    aes(label = year, fill = record),
    size = 5,
    color = "white",
    label.size = 0,
    family = "opensans",
    fontface = "bold",
    alpha = 0.8
  ) +
  expand_limits(y = subtitle_y) +
  scale_fill_manual(
    values = record_cols,
    name   = "Record summer (> 19°C)",
    labels = c("No", "Yes")
  ) +
  scale_color_manual(values = record_cols, guide = "none") +
  labs(
    title = "Does Zurich's Boeoegg Really Predict Summer Heat?",
    subtitle = subtitle_text,
    x = "Boeoegg burn duration (minutes)",
    y = "Avg summer temperature (°C)",
    caption = caption,
  ) +
  theme_minimal(base_size = 10, base_family = "opensans") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 15),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "gray80", size = 0.1),
    plot.margin = margin(t = 12, r = 12, b = 12, l = 12),
    plot.title = element_text(margin = margin(b = 15), size = 40, face = "bold"),
    plot.subtitle = element_textbox_simple(margin = margin(b = 10), size = 20, lineheight = 0.75),
    plot.caption = element_text(margin = margin(t = 10), size = 12, hjust = 0.5),
    panel.spacing.x = unit(12, "pt"),
    panel.spacing.y = unit(12, "pt")
  )
