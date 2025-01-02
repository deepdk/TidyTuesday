library(tidyverse)
library(scales)
library(ggsvg)
library(rsvg)
library(showtext)
library(glue)
library(ggtext)


episode_metrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv')

font_add_google("Rambla","rambla")
showtext_auto()


title <- "Questions and exclamations bring flavor to the dialogue of <span style='color:#e01434'>Bob's Burgers</span>"
st <- glue("Episodes with more questions also tend to have more exclamations, reflecting a heightened conversational or emotional tone.")
cap <- "Data source: {bobsburgersR} R Package Graphic: Deepali Kank"

svg_url <- "https://www.svgrepo.com/download/475098/burger.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

episode_metrics |> 
  mutate(season = as.factor(season)) |> 
  ggplot(aes(question_ratio, exclamation_ratio)) +
  geom_point() +
  geom_point_svg(aes(question_ratio, exclamation_ratio),svg = svg_txt, size = 5) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5) +  # Add a trend line (linear model) without confidence intervals
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_text(color = "black", family = "rambla" , size = 20),
        axis.title.x = element_text(color = "black", family = "rambla" , size = 20),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.y = element_text(color = "black", family = "rambla" , size = 20),
        axis.text.x = element_text(color = "black",family = "rambla",size = 20),
        plot.background = element_rect(fill = "#f8df20", color = "#f8df20"),
        plot.title.position = "plot",
  plot.title = element_textbox_simple(
      colour = "black",
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.5,
      size = 50,
      family = "rambla",
      face = "bold",
    ),
    plot.subtitle = element_textbox_simple(
      colour = "black",
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 1),
      lineheight = 0.5,
      family = "rambla",
      size = 20
    ),
  plot.caption = element_textbox_simple(
      colour = "black",
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 10),
      lineheight = 0.5,
      family = "rambla",
      size = 15
    ),
plot.margin = margin(10, 10, 10, 10)) +
  labs(title = title,
       subtitle = st,
       caption = cap)
