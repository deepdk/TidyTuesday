library(tidyverse)
library(showtext)
library(janitor)
library(scales)
library(lubridate)
library(ggforce)
library(ggtext)


bl_funding <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv")

font_add_google("Nunito", "nunito")
showtext_auto()

font_add_google("Monoton", "mic")
showtext_auto()

bl_funding_long <- bl_funding |>
  pivot_longer(
    cols = c(2:17),
    names_to = "name",
    values_to = "value"
  )


plot_data <- bl_funding_long %>%
  filter(name %in% c("voluntary_gbp_millions", "investment_gbp_millions", "services_gbp_millions")) %>%
  mutate(name = factor(name, levels = c("services_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions")))


bookmark_colors <- c(
  "services_gbp_millions" = "#D9F2F2", # light teal
  "voluntary_gbp_millions" = "#F4D4EB", # light pink
  "investment_gbp_millions" = "#FDF5C9" # light yellow
)


bookmark_bars <- data.frame(
  name = factor(c("services_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions"),
    levels = c("services_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions")
  ),
  xmin = 1997 - 0.5,
  xmax = 2025 + 0.5,
  ymin = 0,
  ymax = 40
)


thread_data <- data.frame(
  name = factor(c("services_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions"),
    levels = c("services_gbp_millions", "voluntary_gbp_millions", "investment_gbp_millions")
  ),
  year = rep(1997, 3),
  value = rep(20, 3)
)

title <- "British Library Funding"

st <- "Annual funding in (£ millions) across three categories from 1998-2023"


cap <- "Data: UK Government | Graphic: Deepali Kank"

ggplot(plot_data, aes(x = year, y = value, fill = name)) +
  geom_rect(
    data = bookmark_bars,
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = name
    ),
    inherit.aes = FALSE,
    alpha = 0.3,
    color = NA
  ) +
  geom_col(width = 0.8, size = 0.3, aes(color = name)) +
  geom_text(aes(label = round(value, 1), y = value + 1.5),
    color = "black", fontface = "bold", size = 2.8
  ) +
  geom_text(
    data = plot_data %>%
      filter(name == "services_gbp_millions", year %in% c(1998, 2023)),
    aes(x = year, y = 1.5, label = year),
    angle = 90,
    hjust = 0,
    vjust = 0.5,
    size = 3,
    family = "nunito",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = bookmark_bars,
    aes(
      x = xmax - 0.5,
      y = ymax - 2,
      label = case_when(
        name == "services_gbp_millions" ~ "Services Funding",
        name == "voluntary_gbp_millions" ~ "Voluntary Funding",
        name == "investment_gbp_millions" ~ "Investment Funding"
      ),
      color = name
    ),
    inherit.aes = FALSE,
    size = 8,
    hjust = 1,
    vjust = 1,
    fontface = "bold",
    family = "mic"
  ) +
  geom_text(
    data = bookmark_bars,
    aes(
      x = xmax - 0.5,
      y = ymax - 5.5,
      label = case_when(
        name == "services_gbp_millions" ~ "Revenue from services, mainly document supply.",
        name == "voluntary_gbp_millions" ~ "Donations & contributed collection items.",
        name == "investment_gbp_millions" ~ "Returns on savings and investments."
      ),
      color = name
    ),
    inherit.aes = FALSE,
    size = 5,
    hjust = 1,
    vjust = 3,
    family = "nunito",
    fontface = "bold"
  ) +
  scale_color_manual(
    values = c(
      "services_gbp_millions" = "#20B2AA", # same as fill
      "voluntary_gbp_millions" = "#DA70D6",
      "investment_gbp_millions" = "#DAA520"
    ),
    guide = "none"
  ) +
  geom_point(
    data = thread_data,
    aes(
      x = year,
      y = value
    ),
    size = 2.2,
    shape = 21, fill = "white",
    color = "black",
    stroke = 0.4,
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = thread_data,
    aes(
      x = year,
      xend = year - 1.2,
      y = value,
      yend = value - 3,
      color = name
    ),
    curvature = 0.4,
    size = 0.5,
    inherit.aes = FALSE
  ) +
  geom_curve(
    data = thread_data,
    aes(
      x = year,
      xend = year - 1.5,
      y = value,
      yend = value + 5,
      color = name
    ),
    curvature = -0.5,
    size = 0.5,
    inherit.aes = FALSE
  ) +
  facet_wrap(~name,
    nrow = 3,
    labeller = as_labeller(c(
      "services_gbp_millions" = "Services Funding",
      "voluntary_gbp_millions" = "Voluntary Funding",
      "investment_gbp_millions" = "Investment Funding"
    ))
  ) +
  scale_fill_manual(
    values = c(
      "services_gbp_millions" = "#20B2AA",
      "voluntary_gbp_millions" = "#DA70D6",
      "investment_gbp_millions" = "#DAA520"
    ),
    name = "Funding Type"
  ) +
  scale_color_manual(
    values = c(
      "services_gbp_millions" = "#20B2AA",
      "voluntary_gbp_millions" = "#DA70D6",
      "investment_gbp_millions" = "#DAA520"
    ),
    guide = "none"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      margin = margin(b = 10),
      size = 40,
      vjust = 0.1,
      face = "bold",
      family = "nunito",
      halign = 0.5,
      color = "#006d77"
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(b = 15),
      size = 20,
      family = "nunito",
      halign = 0.5,
      color = "#006d77"
    ),
    plot.caption = element_textbox_simple(
      margin = margin(t = 10),
      size = 10,
      family = "nunito",
      halign = 0.5
    ),
    strip.text = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    plot.background = element_rect(fill = "#f8f9fa", color = NA)
  ) +
  labs(title = title, subtitle = st, caption = cap)
