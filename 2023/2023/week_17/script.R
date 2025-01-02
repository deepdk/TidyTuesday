library(tidyverse)
library(scales)
library(janitor)
library(showtext)


font_add(family = "bold", regular = "/kaggle/input/bold-font/theboldfont.ttf")
showtext_auto()

winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-25/london_marathon.csv')

winners <- winners%>%
clean_names()

winners %>%
group_by(category, nationality)%>%
count(nationality, sort = TRUE)%>%
ggplot(aes(nationality,n,color = category))+
geom_point(size = 18)+
geom_text(aes(label = n), color = "black", size = 12)+
scale_color_manual(values = c("#fcf6bd","#a9def9","#e4c1f9","#d0f4de"))+
coord_flip()+
theme_minimal()+
theme(legend.position  = "top")+
theme(legend.text = element_text(size = 20, face = "bold", color="white", family = "bold"))+
theme(legend.title = element_text(size = 20, face = "bold", color="white", family = "bold"))+
theme(legend.justification = "center")+
theme(plot.background  = element_rect(fill="#a33d28", color="#a33d28"))+
theme(axis.title.x     = element_blank()) +
theme(axis.title.y     = element_blank()) +
theme(axis.text.x      = element_blank()) +
theme(axis.text.y      = element_text(size=25, color = "white", face = "bold", family = 'bold')) +
theme(plot.title       = element_text(color="white", size=75, face = "bold",hjust = 0.5, family = 'bold'))+
theme(plot.caption     = element_text(color="white", size=20, face = "bold", hjust = 0.5, family = 'bold'))+
labs(title = "London Marathon Winners (1981-2022)",
    caption = "Data Source:LondonMarathonRPackage by @nrennie Graphic: Deepali Kank")
