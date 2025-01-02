library(tidyverse)
library(scales)
library(showtext)
library(stringr)
library(ggchicklet)

grants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-10-03/grants.csv')

nat_df <- grants[str_detect(grants$agency_name, "^National"), ]
nat_df

nat_df |>
  filter(!is.na(agency_name)) |> 
  filter(!is.na(estimated_funding)) |>
  group_by(agency_name) |> 
  summarise(total = sum(estimated_funding)) |> 
  arrange(desc(total))

nat_df |>
  filter(!is.na(agency_name)) |> 
  filter(!is.na(estimated_funding)) |>
  group_by(agency_name) |> 
  summarise(total = sum(estimated_funding)) |> 
  ggplot(aes(reorder(agency_name, total), total)) + 
  geom_chicklet(radius = grid::unit(12, "mm"),fill = "#0353a4", alpha = 0.7) + 
  scale_y_log10(labels = comma) +
  coord_flip() + 
  theme_minimal() + 
  theme(axis.text.x = element_blan()) + 
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = "none")

# Added the text, titel, subtitle in Figma
