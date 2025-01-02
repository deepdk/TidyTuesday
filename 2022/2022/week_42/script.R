library(tidyverse) 
library(MetBrewer)
library(scales)
library(lubridate)
library(dplyr)
library(showtext)
library(showtext)
library(stringr)
library(tidytext)
library(reshape2)
library(forcats)

library(showtext)
font_add_google('Fira Sans', 'firasans')
font_add(family = "Horror",regular = "../input/dead-font/Dead Font Walking.otf")
showtext_auto()

dialogue <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')
bing <- read.csv("../input/bing-data/Bing.csv")
nrc <- read.csv("../input/nrc-data/NRC.csv")

dialogue <- dialogue %>% 
   mutate(across(where(is.character), tolower)) %>% 
  clean_names()

tokens <- dialogue %>%  
  mutate(dialogue=as.character(dialogue)) %>%
  unnest_tokens(word, dialogue)

sentiments <- tokens %>% 
  inner_join(nrc, "word") %>%
  group_by(word,season)%>%
  count(word, sentiment, sort=TRUE) 
  
sentiments <- sentiments %>%
mutate(season = case_when(season ==1~"season 1",
                          season == 2~"season 2",
                          season == 3~"season 3",
                          season == 4~"season 4"))

  

p1 <- sentiments %>%
ggplot(aes(x=as.factor(sentiment), y=n,  fill=sentiment)) +      
geom_bar(stat="identity", size = 5) +
ylim(-50,100) +
scale_fill_manual(values = met.brewer("Java",10))+
coord_polar()+
facet_wrap(~season)+
theme_light()+
theme(panel.background = element_rect(fill="black", color="black")) +
theme(plot.background  = element_rect(fill="black", color="black")) +
theme(panel.border     = element_rect(color="black")) +
theme(strip.background = element_rect(fill="black", color="black")) +
theme(strip.background = element_rect(color="black", fill="red", size=1, linetype="solid"))+
theme(strip.text.x = element_text(size = 50, color = "black", family = "Horror"))+
theme(axis.text.x = element_text(size = 20, color = "white", family = "firasans"))+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.y  = element_text(size=20, color = "#FFFFFF", family = 'firasans'))+
theme(plot.title   = element_text(color="#FFFFFF", size=60, face = "bold", hjust = 0.5, family = 'firasans'))+
theme(plot.subtitle = element_text(color="red", size=60,hjust = 0.5, family = 'Horror', face = "bold"))+
theme(plot.caption = element_text(color="#FFFFFF", size=20,hjust = 0.5, family = 'Horror', face = "bold"))+
theme(legend.position = "none")+
labs(subtitle = "Sentiment Analysis", 
    caption = "Data source : Stranger Things Dialogue/TidyTuesday/week 42 2022 \n Graphic : Deepali Kank")
