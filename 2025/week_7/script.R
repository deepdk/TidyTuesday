library(tidyverse)
library(scales)
library(janitor)
library(showtext)
library(usmap)
library(mapproj)
library(ggtext)

font_add_google("Outfit","outfit")
showtext_auto()


agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')


view(agencies)

state = map_data("state")
head(state)

agencies <- agencies |> 
  filter(!is.na(longitude),
         !is.na(latitude),
         !is.na(agency_type),
         between(longitude, -130, -60),  # Approximate range for USA
         between(latitude, 20, 55))      # Approximate range for USA



title <- "Agencies Reporting Crime Data: <span style='color: #6c9a8b;'>Active</span> vs. <span style='color:#e8998d;'>Absent</span> in the FBI’s NIBRS"

st <- "While most law enforcement agencies across the U.S. participate in the FBI’s National Incident-Based Reporting System (NIBRS), significant gaps remain, especially in parts of the South and East Coast, where many agencies have yet to adopt the system."

cap <- "Data source: FBI Crime Data API Graphic: Deepali Kank"





p1 <- 
  ggplot()+
  geom_polygon(
        data = state, aes(x = long, y = lat, group = group)
    )+
  geom_point(
        data = agencies, 
        aes(x = longitude, y = latitude, color = is_nibrs),shape = 19, alpha = .2, size = 1
    ) +
 scale_color_manual(values = c("#e8998d","#6c9a8b"))+
  coord_map("conic", lat0 = 30)+
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
  plot.title = element_textbox_simple(
      colour = "black",
      hjust = 0,
      halign = 0,
      margin = margin(b = 10, t = 0),
      lineheight = 0.6,
      size = 40,
      family = "outfit",
      face = "bold",
    ),
    plot.subtitle = element_textbox_simple(
      colour = "black",
      hjust = 0,
      halign = 0,
      margin = margin(b = 15, t = 1),
      lineheight = 0.6,
      family = "outfit",
      size = 15
    ),
  plot.caption = element_textbox_simple(
      colour = "black",
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 30),
      lineheight = 0.5,
      family = "outfit",
      size = 15
    ),
  plot.margin = margin(10, 10, 10, 10)) +
  labs(title = title,
       subtitle = st,
       caption = cap)



p1
