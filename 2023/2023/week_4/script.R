library(tidyverse)
library(scales)
library(janitor)
library(lubridate)
library(alone)
library(eyedroppeR)
library(showtext)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Bold.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#839CA9"
  color.text = "#030303"
  
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
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=20, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df <- loadouts %>% 
  select(-season,-version)

final <- left_join(survivalists, loadouts,on = "name")
head(final)

p1 <- final %>% 
  select(season,name, item,result) %>% 
  filter(result == 1) %>% 
  group_by(name, item) %>% 
  ggplot(aes(name, item, color = name))+
  geom_point()+
  theme_minimal()+
  geom_text(aes(label = item), size = 8, family = "Roboto", color = "#353D47")+
  #scale_color_manual(values = met.brewer("Degas",10))+
  my_theme()+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(plot.margin = unit(c(15, 0.2, 3, 0.35), "cm"))
p1

# Added Images and title using figma
