library(tidyverse)
library(showtext)
library(usmap)

library(repr)
options(repr.plot.width = 20, repr.plot.height =13)

annotate <- ggplot2::annotate

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
font_add(family = "Horror", regular = "D:/HP laptop/Fonts/bloodlust.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text ="#F2F2F2"
  
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
    
    # Format the legend
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=150, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=100, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=50, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

state_stations <- state_stations %>%
  mutate(state = str_to_lower(state)) %>% 
  rename(region = state)

news <- state_stations %>% 
  filter(format == "News/Talk") %>% 
  count(region, sort = TRUE)
  
state = map_data("state")

cor <- state %>%
  select(-group, -order)%>%
  group_by(region) %>%
  summarise(lan = mean(long),
            latt = mean(lat))
            
 news <- news %>% 
  left_join(cor, on = 'region')
  
 ggplot() +
  geom_polygon(data = state, aes(long, lat, group = group), fill = '#0F0F0F', color = '#F5F5F5') +
  geom_point(data = news, aes(x = lan, y = latt), color = '#C0FF3E',size = 22) +
  geom_text(data = news, aes(label = n, x = lan, y = latt), 
            size = 18, color = 'black', fontface = 'bold')+
  my_theme()+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(legend.position = 'none')+
  labs(title = "Radio Stations in USA",
       subtitle = "No.of radio stations with News/Talk format",
       caption = "Data Source : Radio Stations/TidyTuesday/week 45 2022\nGraphic : Deepali Kank") 
