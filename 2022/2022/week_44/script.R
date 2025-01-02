library(tidyverse)
library(showtext)
library(scales)
library(ggimage)
library(ggthemes)

library(repr)
options(repr.plot.width = 20, repr.plot.height =20)

annotate <- ggplot2::annotate

font_add(family = "Roboto", regular = "../input/roboto/RobotoCondensed-Regular.ttf")
font_add(family = "Horror", regular = "../input/bloodlust/bloodlust.ttf")
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
    theme(plot.title       = element_text(color="#FF0000", size=130, face = "bold", hjust = 0.5, family = 'Horror'))+
    theme(plot.subtitle    = element_text(color="#FF0000", size=50, face = "bold", hjust = 0.5, family = 'Horror'))+
    theme(plot.caption     = element_text(color="#FF0000", size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = "#FF0000", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = "#FF0000", face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +     
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

base_url<-'https://www.themoviedb.org/t/p/w1280'

df <- df %>% 
  mutate(images = paste0(base_url, poster_path))
  
df %>% 
  arrange(desc(popularity)) %>% 
  head(20)%>%
  ggplot(aes(fct_reorder(title, vote_average), vote_average))+
  geom_bar(stat = 'identity', width = 0.02, fill ="#FF0000" )+ 
geom_image(mapping=aes(y=vote_average, x=title, image=images,  color="#FF0000"), size=0.059, asp=0.5)+
geom_image(mapping=aes(y=vote_average, x=title, image=images), size=0.05, asp=0.5)+
geom_text(aes(label = tagline), size = 11,hjust = 2, vjust = -1,color = '#FF0000', fontface = "bold", family = "Horror")+
  coord_flip()+
  my_theme()+
  theme(axis.title.y = element_blank())+
  labs(title = "Top 20 Popular Horror Movies",
       subtitle = "1950 - 2022 by Vote Average",
      caption = "Data Source : Horror Movies/TidyTuesday/week 44 2022")  
