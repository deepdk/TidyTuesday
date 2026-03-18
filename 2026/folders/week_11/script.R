#Libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(here)
library(ggtext)
library(showtext)
library(svglite)
library(ggtext)
library(sysfonts)

font_add_google("PT Sans", "PT Sans")
showtext_auto()

monthly_losses <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-17/monthly_losses_data.csv"
) |>
  clean_names()

losses <- monthly_losses |> 
  filter(geo_group == "county", species == "salmon") |> 
  select(date,region, dead,discarded,escaped,other) |> 
  pivot_longer(cols = c(3:6),
               names_to = "name",
               values_to = "value")

losses <- losses |> 
  mutate(date = as.Date(date),
         year = year(date),
         month = month(date),
         day = day(date))

losses_slope <- losses |>
  group_by(region, name, year) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  group_by(region, name) |>
  filter(year %in% c(min(year), max(year))) |>
  ungroup()


losses_dead <- losses_slope |>
  filter(name == "dead") |>
  mutate(
    x = c(1, 1.22)[match(year, c(2020, 2025))],   # tighter distance
    value_lbl = label_number(scale = 1e-6, suffix = "M", accuracy = 0.1)(value)
  )

ggplot(losses_dead, aes(x = x, y = value, group = 1)) +
  geom_ribbon(
    aes(ymin = 0, ymax = value),
    fill = "#E76F61",
    alpha = 0.14
  ) +
  geom_line(
    color = "#E76F61",
    linewidth = 1.6,
    lineend = "round"
  ) +
  geom_point(
    shape = 21,
    size = 3.8,
    stroke = 0.8,
    fill = "#E76F61",
    color = "#E76F61"
  ) +
  geom_text(
    aes(
      label = value_lbl,
      x = if_else(year == 2020, x + 0.008, x - 0.008)
    ),
    vjust = -0.9,
    size = 5,
    family = "PT Sans",
    fontface = "bold",
    color = "#3A3A3A"
  ) +
  facet_wrap(~region, ncol = 3) +
  scale_x_continuous(
    breaks = c(1, 1.22),
    labels = c("2020", "2025"),
    limits = c(0.97, 1.25),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  labs(
    title = "<span style='color:#E76F61;'>Dead salmon</span> Losses Surge in Norway's Biggest Producing Regions",
    subtitle = paste0(
      "Nordland and Trøndelag, among the largest aquaculture counties — both reported millions more dead fish in 2025 than five years prior"
    ),
    x = NULL,
    y = NULL,
    caption = "Source: Norwegian Veterinary Institute / Laksetap API"
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "PT Sans", base_size = 13) +
  theme(
    plot.background = element_rect(fill = "#f8f9fa"),
    plot.title = ggtext::element_textbox_simple(
      size = 30,
      face = "bold",
      lineheight = 0.7,
      margin = margin(0, 0, 10, 0),
      padding = margin(0, 0, 0, 0),
      fill = NA,
      box.color = NA
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 20,
      color = "#555555",
      lineheight = 0.8,
      margin = margin(0, 0, 16, 0),
      padding = margin(0, 0, 0, 0),
      fill = NA,
      box.color = NA
    ),
    plot.caption = element_text(
      size = 13,
      color = "#777777",
      hjust = 0
    ),
    strip.text = element_text(
      face = "bold",
      size = 17,
      color = "#2C2C2C"
    ),
    panel.grid = element_blank(),
    #panel.grid.major.y = element_line(color = "#E6E6E6", linewidth = 0.45),
    panel.grid.major.x = element_line(color = "#e9ecef", linewidth = 0.6),
    axis.text.x = element_text(
      size = 11.5,
      face = "bold",
      color = "#4A4A4A"
    ),
    axis.text.y = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    plot.margin = margin(18, 20, 15, 20)
  )
