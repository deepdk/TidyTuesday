library(tidyverse) 
library(scales) 
library(MetBrewer)
library(lubridate)
library(dplyr)
library(showtext)
library(ggthemes)
library(ggridges)
library(showtext)

yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')

font_add(family = "Modern","Fonts/Lemosty.otf")
showtext_auto()

yarn %>%
filter(!is.na(yarn_weight_name))%>%
filter(yarn_weight_name != 'No weight specified') %>% 
ggplot(aes(rating_average,yarn_weight_name, fill = yarn_weight_name))+
geom_density_ridges2()+
  scale_fill_manual(values = met.brewer("Cross",15))+
theme_light()+
theme(panel.background = element_rect(fill="#292929", color="#292929")) +
    theme(plot.background  = element_rect(fill="#292929", color="#292929")) +
    theme(panel.border     = element_rect(color="#292929")) +
theme(panel.grid.major.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
    theme(panel.grid.minor.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
    theme(panel.grid.major.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL))+
    theme(panel.grid.minor.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
    theme(strip.background = element_rect(fill="#292929", color="#292929"))+
theme(plot.title       = element_text(color="#FFFFFF", size=150, face = "bold", family = 'Modern'))+
    theme(plot.subtitle    = element_text(color="#FFFFFF", size=80, face = "bold", family = 'Modern'))+
    theme(plot.caption     = element_text(color="#FFFFFF", size=50, face = "bold", hjust = 0.5, family = 'Modern'))+
theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=80, color = "#FFFFFF", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Modern')) +
    theme(axis.text.y      = element_text(size=80, color = "#FFFFFF", face = "bold", family = 'Modern'))+
theme(legend.position = 'none')+
  labs(title = "Rating Distribution of Yarn Weight Categories on Ravelry",
       subtitle = "Ravelry is a social networking and organizational tool for knitters, crocheters, designers, spinners,\nweavers and dyers",
       caption = "Data Source/ravelry.com/TidyTuesday/week 41 2022 \n Graphic : Deepali Kank")
