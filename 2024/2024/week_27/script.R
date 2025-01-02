library(tidyverse)
library(janitor)
library(scales)
library(MetBrewer)
library(ggtext)
library(ggrepel)
library(showtext)

font_add_google("Barlow","Roboto")
showtext_auto()

tt_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-07-02/tt_datasets.csv')

tt_datasets |> 
  mutate(dataset_name = str_replace_all(dataset_name, "_", " ")) |> 
  mutate(dataset_name = str_to_title(dataset_name)) |> 
  ggplot(aes(variables, observations, 
             color = variables >= 50, alpha = 0.9)) + 
  geom_point(aes(size = 3)) + 
  geom_text_repel(aes(label = ifelse(variables >= 50, dataset_name, "")), size = 4,family = 'Roboto',fontface = "bold", nudge_y = 0.2, check_overlap = TRUE) +
  scale_color_manual(values = c("#0f4c5c","#9a031e")) +
  scale_y_log10(label = comma) +
  geom_curve(
    aes(x = 120, y = 4000, xend = 129, yend = 20000),
    arrow = arrow(length = unit(0.01, "npc")),
    colour = "black"
  ) + 
  geom_textbox(
    aes(x = 115, y = 4000, label = "The OWID Energy Dataset from 2023 has 129 variables."),
    size = 4,
    width = unit(2, "in"),
    box.color = "white",
    fill = "white",
    halign = 0.5,
    valign = 0.5,
    color = "black",
    alpha = 0.5,
    family = 'Roboto',
    fontface = "bold"
  ) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  labs(title = "Mapping Observations vs. Variables in TidyTuesday Datasets",
       subtitle = "Distribution of 644 Datasets (2018-2024) by Number of Observations and Variables, highlighting those with **<span style='color:#9a031e;'>more than 50 variables</span>**.<br> While most datasets have fewer variables, there are notable datasets with higher complexity and a wide range of observation counts.",
       caption = "Data Source: {ttmeta} package, Graphic: Deepali Kank")
