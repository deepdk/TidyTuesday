library(tidyverse)
library(lubridate)
library(scales)
library(janitor)
library(ggthemes)
library(MetBrewer)
library(showtext)
library(ggsvg)

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-26/richmondway.csv')

svg_url <- "https://www.svgrepo.com/download/404478/bomb-dynamite-dynamite-bomb-explode-firework-bomb.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
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
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=15, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=15, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df |> 
  mutate(Episode = as.factor(Episode)) |> 
  ggplot(aes(Episode, F_count_RK)) +
  geom_point()  +
  geom_point_svg(aes(Episode, F_count_RK),svg = svg_txt, size = 25) + 
  #geom_segment(aes(x=Episode, xend=Episode, y=0, yend=F_count_RK),size = 1) +
  geom_text(aes(label = F_count_RK), size = 4, color = 'black', hjust = 0.5, vjust = 0.5, fontface = "bold") + 
  facet_wrap(~Season) + 
  scale_fill_manual(values = met.brewer("Signac",2)) + 
  scale_color_manual(values = met.brewer("Signac",2)) + 
  scale_y_reverse() +
  my_theme()+
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank())

# Added the title, subtitle and image in Figma

