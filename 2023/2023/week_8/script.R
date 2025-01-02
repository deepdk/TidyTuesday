library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(lubridate)
library(showtext)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#CFCFCF"
  color.text ="#030303"
  
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
    theme(strip.text       = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

# separte the colors in excel and updated the dataset to df
df <- df %>% pivot_longer(cols= c('color_hex1','color_hex2','color_hex3','color_hex4','color_hex5',
  'color_hex6','color_hex7','color_hex8','color_hex9','color_hex10','color_hex11','color_hex12','color_hex13','color_hex14','color_hex15'),                          
                            
                    names_to='color_hex',
                    values_to='hex_code')
                    
df %>%
  filter(painting_title %in% c("A Walk in the Woods","Winter Mist","Spectacular Waterfall","Windy Waves","Ebony Sea","Meadow Lake","Autumn Splendor","Covered Bridge","Arizona Splendor","Country Cabin","Country Cabin","Ocean Sunrise","Portrait of Winter","Purple Splendor","Contemplative Lady"))%>%
  ggplot(aes(reorder(painting_title,num_colors),num_colors,  fill = I(hex_code))) +
  geom_col(position = "fill") +
  my_theme()+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(plot.margin = unit(c(2, 0.2, 3, 0.35), "cm"))
  
  ## Added the images, labels and title in figma
                    
