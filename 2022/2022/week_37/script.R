library(tidyverse) 
library(scales) 
library(patchwork)
library(MetBrewer)
library(usmap)
library(dplyr)
library(ggtext) 
library(lubridate)
library(showtext)
library(tidytext) 
library(SnowballC)
library(ggwordcloud) 
library(wordcloud2)
library(repr)
options(repr.plot.width = 20, repr.plot.height =13)

font_add(family = "Pally",regular = "../input/pally-bold/Pally-Bold.otf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text = "#FFF5EE"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +

# Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=60, face = "bold", hjust = 0.5, family = 'Pally'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Pally'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Pally'))+
    theme(axis.title.x     = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Pally'))+
    theme(axis.title.y     = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Pally')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Pally')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Pally')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Pally')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df <- read.csv("../input/bigfoot/bfro_reports_geocoded.csv")

final <- df1 %>%
filter(year >= 2000)%>%
filter(classification != 'Class C')%>%
filter(state != "Alaska")%>%
filter(longitude >= -130)

state = map_data("state")

year_2000_fall <- final %>%
  filter(season == "Fall")
year_2000_winter <- final %>%
  filter(season == "Winter")
year_2000_summer <- final %>%
  filter(season == "Summer")
year_2000_spring <- final %>%
  filter(season == "Spring")
  
p1 = 
  ggplot()+
  geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#030303", size = 0.1, alpha = .3
    )+
  geom_point(
        data = year_2000_fall, 
        aes(x = longitude, y = latitude, color = classification),
        shape = 19, alpha = .5, size = 3
    ) +
 scale_color_manual(values = c("#FFFF00","#00F5FF"))+
  coord_map()+
my_theme()+
theme(axis.text.x = element_blank())+
theme(axis.text.y = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+ 
labs(title = "Fall")+
theme(legend.position = "top") +
theme(legend.key = element_rect(fill = "#030303"))+
theme(legend.text = element_text(size = 40, face = "bold", color = 'white', family = "Pally"))+
theme(legend.justification = "center")+
theme(legend.title = element_blank())+
theme(legend.key.size = unit(1, 'cm'))+
theme(legend.background = element_rect(fill = "transparent"))+
guides(color = guide_legend(override.aes = list(size = 10)))

p2 = 
  ggplot()+
  geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#030303", size = 0.1, alpha = .3
    )+
  geom_point(
        data = year_2000_winter, 
        aes(x = longitude, y = latitude, color = classification),
        shape = 19, alpha = .5, size = 3
    ) +
 scale_color_manual(values = c("#FFFF00","#00F5FF"))+
  coord_map()+
my_theme()+
theme(axis.text.x = element_blank())+
theme(axis.text.y = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
labs(title = "Winter")+
theme(legend.position = "top") +
theme(legend.key = element_rect(fill = "#030303"))+
theme(legend.text = element_text(size = 40, face = "bold", color = 'white', family = "Pally"))+
theme(legend.justification = "center")+
theme(legend.title = element_blank())+
theme(legend.key.size = unit(1, 'cm'))+
theme(legend.background = element_rect(fill = "transparent"))+
guides(color = guide_legend(override.aes = list(size = 10)))

p3 = 
  ggplot()+
  geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#030303", size = 0.1, alpha = .3
    )+
  geom_point(
        data = year_2000_summer, 
        aes(x = longitude, y = latitude, color = classification),
        shape = 19, alpha = .5, size = 3
    ) +
 scale_color_manual(values = c("#FFFF00","#00F5FF"))+
  coord_map()+
my_theme()+
theme(axis.text.x = element_blank())+
theme(axis.text.y = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
labs(title = "Summer")+
theme(legend.position = "top") +
theme(legend.key = element_rect(fill = "#030303"))+
theme(legend.text = element_text(size = 40, face = "bold", color = 'white', family = "Pally"))+
theme(legend.justification = "center")+
theme(legend.title = element_blank())+
theme(legend.key.size = unit(1, 'cm'))+
theme(legend.background = element_rect(fill = "transparent"))+
guides(color = guide_legend(override.aes = list(size = 10)))

p4 = 
  ggplot()+
  geom_polygon(
        data = state, aes(x = long, y = lat, group = group), 
        color = "gray95", fill = "#030303", size = 0.1, alpha = .3
    )+
  geom_point(
        data = year_2000_spring, 
        aes(x = longitude, y = latitude, color = classification),
        shape = 19, alpha = .5, size = 3
    ) +
 scale_color_manual(values = c("#FFFF00","#00F5FF"))+
  coord_map()+
my_theme()+
theme(axis.text.x = element_blank())+
theme(axis.text.y = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
labs(title = "Spring")+
theme(legend.position = "top") +
theme(legend.key = element_rect(fill = "#030303"))+
theme(legend.text = element_text(size = 40, face = "bold", color = 'white', family = "Pally"))+
theme(legend.justification = "center")+
theme(legend.title = element_blank())+
theme(legend.key.size = unit(1, 'cm'))+
theme(legend.background = element_rect(fill = "transparent"))+
guides(color = guide_legend(override.aes = list(size = 10)))

options(repr.plot.width = 20, repr.plot.height =13)

P = (p1+p2)/(p3+p4)

P + plot_annotation(title = "Big Foot Sightings by Season 2000-2022",
                    theme = theme(plot.title = element_text(size = 60, face = "bold", hjust = 0.5, color = "#FFFFFF", family = 'Pally'),
                                  plot.background  = element_rect(fill="#030303", color="#030303"),
                                  legend.position = "bottom"))+
plot_layout(guides = "collect", widths = 3, heights = 1)
