library(tidyverse)
library(MetBrewer)
library(janitor)
library(scales)
library(lubridate)
library(showtext)

languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

font_add(family = "collegiate",regular = "D:/HP laptop/Fonts/collegiate/CollegiateFLF.ttf")
showtext_auto()

languages %>% 
  filter(appeared >= 1990) %>% 
  count(appeared) %>% 
  ggplot(aes(appeared,n))+
  geom_line(size = 2, color = "#219ebc")+
  geom_point(color = "#FFDAB9",size = 13)+
  geom_text(aes(label = n), size = 5, vjust = 0.5,hjust = 0.5, color = "black", fontface = "bold")+
  theme_light()+
theme(panel.background = element_rect(fill="black", color="black")) +
theme(plot.background  = element_rect(fill = "black", color="black")) +
theme(panel.border     = element_rect(color="black")) +
theme(panel.grid.major.y = element_line(colour = 'black', linewidth = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.minor.y = element_line(colour = 'black', linewidth = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.major.x = element_line(colour = 'black', linewidth = 1, linetype = NULL, lineend = NULL))+
theme(panel.grid.minor.x = element_line(colour = 'black', linewidth = 1, linetype = NULL, lineend = NULL)) +
theme(strip.background = element_rect(fill="black", color="black"))+
theme(plot.title       = element_text(color="#219ebc", size=70, face = "bold", family = 'collegiate'))+
theme(plot.subtitle    = element_text(color="#219ebc", size=40, face = "bold", family = 'collegiate'))+
theme(plot.caption     = element_text(color="#219ebc", size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
theme(axis.title.x     = element_blank()) +
theme(axis.title.y     = element_blank()) +
theme(axis.text.x      = element_text(size=25, color = "#219ebc", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.text.y      = element_text(size=25, color = "#219ebc", face = "bold", family = 'Roboto'))+
  labs(title = "Rise of the Programming Languages",
       subtitle = "Number of languages first appeared from 1990-2023")
       
# Annotation added using Figma       
