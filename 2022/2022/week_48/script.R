library(tidyverse)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(janitor)
library(dplyr)
library(ggflags)

font_add(family = "Roboto", regular = "RobotoCondensed-Bold.ttf")
showtext_auto()

worldcups <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')

worldcups <- worldcups %>% 
  rename(country = host)
  
c_codes = read_csv("D:/country-codes.csv")
c_codes <- c_codes %>% 
  clean_names()
  
c_codes <- c_codes %>% 
  rename(country = english_short_name_lower_case)
  
worldcups <- worldcups %>% 
  mutate(country = recode(str_trim(country), "USA" = "United States")) %>% 
  mutate(country = recode(str_trim(country), "England" = "United Kingdom"))%>%
  mutate(country = recode(str_trim(country), "Japan, South Korea" = "Japan"))
  
final <- left_join(worldcups, c_codes,by = 'country')

final %>%
mutate(alpha_2_code = str_to_lower(alpha_2_code),
        alpha_2_code = as.character(alpha_2_code))%>%
ggplot(aes(year, attendance))+
geom_line(alpha = 0.5,size=3.5, color = "#00CD00")+
geom_point()+
geom_flag(aes(country = alpha_2_code), size = 18)+
scale_y_continuous(labels = comma)+
geom_text(aes(x = 1930, y = 434000, label = "winner"), size = 15, vjust = 3, fontface = "bold", family = "Roboto",color = '#FFB90F') +
geom_text(aes(x = 1934, y = 395000, label = "winner"),size = 15, vjust = 3, fontface = "bold", family = "Roboto", color = '#FFB90F') +
geom_text(aes(x = 1978, y = 1610215, label = "winner"),size = 15, vjust = 3, fontface = "bold", family = "Roboto", color = '#FFB90F')+
geom_text(aes(x = 1998, y = 2859234, label = "winner"),size = 15, vjust = 3, fontface = "bold", family = "Roboto", color = '#FFB90F') +  
theme_light()+
theme(panel.background = element_rect(fill="#292929", color="#292929")) +
theme(plot.background  = element_rect(fill="#292929", color="#292929")) +
theme(panel.border     = element_rect(color="#292929")) +
theme(panel.grid.major.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.minor.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.major.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL))+
theme(panel.grid.minor.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(strip.background = element_rect(fill="#292929", color="#292929"))+
theme(plot.title       = element_text(color="#FFB90F", size=200, face = "bold", family = 'Roboto'))+
theme(plot.subtitle    = element_text(color="#FFB90F", size=100, face = "bold", family = 'Roboto'))+
theme(plot.caption     = element_text(color="#FFB90F", size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
theme(axis.title.x     = element_text(size=50, color = "#FFB90F", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.title.y     = element_text(size=50, color = "#FFB90F", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.text.x      = element_text(size=50, color = "#FFB90F", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.text.y      = element_text(size=50, color = "#FFB90F", face = "bold", family = 'Roboto'))+
labs(title = "FIFA World Cup Host Countries",
    subtitle = "Uruguay, Italy, Argentina and France were the host countries and winners \n1930-2018",
    caption = "Data Source: FIFA Worldcup/TidyTuesday week 49 2022")

