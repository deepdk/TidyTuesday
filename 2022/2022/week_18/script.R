library(tidyverse)
library(paletteer)
library(mice)
library(showtext)
library(patchwork)
library(naniar)
library(tidytext)
library(reshape2)
library(ggrepel)
library(ggtext)

capacity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/capacity.csv')
average_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/average_cost.csv')

font_add_google('Fira Sans', 'firasans')
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#0f3051"
  color.text = "#FFFFFF"
  
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
    theme(legend.position = "top") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 25, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",
                                    color = "#FFFFFF",
                                    size = 25, face = "bold"))+
                                    
   # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.title.y     = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.x      = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=17, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

p1 <- capacity %>%
  mutate(type2 = tidytext::reorder_within(type, -total_gw, within = year))%>%
  ggplot(aes(x = year, weight = total_gw, fill = type, colour = type)) +
  geom_bar(position = "dodge", aes(group = type2),color = "black") +
  scale_x_discrete(limits = c(2014:2020))+
  scale_fill_paletteer_d("ggprism::prism_light")+
  labs(title = "Utility Scale Solar 2021 Edition",
      subtitle = "Total Capacity of Energy in gigawatts",
      x = "Year",
      y = "Total_GW",
      color = "Type\nof\npower:")+
  my_theme()
p1

average_cost <- melt(average_cost ,  id.vars = 'year', variable.name = 'series')

p2 <- ggplot(average_cost, aes(year, value)) +
  geom_line(aes(colour = series))+
  geom_point(color = "white", size = 2)+
  scale_x_discrete(limits = c(2009:2021))+
  scale_fill_paletteer_d("ggprism::prism_light")+
  labs(title = "Average cost for each type of power in dollars/MWh",
      x = "",
      y = "",
      )+
  my_theme()
p2

p1/p2
