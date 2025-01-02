library(tidyverse)
library(lubridate)
library(scales)
library(janitor)
library(ggthemes)
library(MetBrewer)
library(showtext)
library(geofacet)

font_add(family = "Roboto", regular = "Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

states <- read_csv("states.csv")

states |>
  filter(sector %in% c("Construction","Manufacturing")) |> 
  group_by(year, sector, state) |> 
  summarise(total = sum(p_members)) |> 
  ggplot(aes(year, total, fill = sector)) + 
  geom_area() + 
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c("#00CDCD","#FFB90F")) + 
  facet_geo(~ state,grid = "us_state_grid2", label = "name") + 
  theme_light() + 
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank())+
  theme(axis.ticks       = element_blank()) + 
  theme(panel.background = element_rect(fill="black", color="black")) +
  theme(plot.background  = element_rect(fill="black", color="black")) +
  theme(panel.border     = element_rect(color="black")) +
  theme(strip.background = element_rect(fill="black", color="black"))+
  theme(strip.background = element_rect(color="black", fill="#E0EEEE", size=1, linetype="solid"))+
  theme(strip.text.x = element_text(size = 30, color = "black", family = "Roboto"))+
  theme(axis.text.x = element_text(size = 20, color = "white", family = "Roboto"))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y  = element_text(size=20, color = "#FFFFFF", family = 'Roboto'))+
  theme(plot.title   = element_text(color="#FFFFFF", size=60, face = "bold", hjust = 0.5, family = 'Roboto'))+
  theme(plot.subtitle = element_text(color="#FFFFFF", size=30,hjust = 0.5, family = 'Roboto', face = "bold"))+
  theme(legend.position = "none")

# Added the title, subtitle and caption in figma
