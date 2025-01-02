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
library(lubridate)

library(repr)
options(repr.plot.width = 20.0, repr.plot.height = 30)

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Stephan King's Books
sk <- nyt_titles %>%
  filter(author == 'Stephen King')
sk

sk_top20 <- sk  %>%
  filter(total_weeks >= 10)%>%
  arrange(desc(total_weeks))
  
font_add(family = "Horror", regular = "../input/horror-font/Nightmare Before Christmas.ttf")
font_add(family = "Serif", regular = "../input/gonjuring-font/THE GONjURING.ttf")
font_add(family = "Halloween", regular = "../input/damned/Damned.ttf")
showtext_auto()
  
  my_theme1 <- function() {
  
  # Colors
  color.background = "#000000"
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
    theme(legend.position = "none") +
    
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=60, face = "bold", hjust = 0.5, family = "Halloween"))+
    theme(plot.subtitle    =  element_text(color = color.text, size=40, face = "bold", hjust = 0.5, family = "Horror"))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=20, color = color.text)) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_blank()) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

sf <- ggplot(sk_top20, aes(fct_reorder(title, total_weeks), total_weeks, fill = fct_reorder(title,total_weeks)))+
  geom_col()+
  geom_bar(stat = "identity") +
  geom_text(aes(label = title), hjust = 1.2, color = "white",size = 14.8, family = "Serif", fontface = 'bold')+
  coord_flip()+
  scale_fill_paletteer_d("khroma::batlow")+
  labs(title = "Stephen King's Popular Books")+
  my_theme1()
sf
