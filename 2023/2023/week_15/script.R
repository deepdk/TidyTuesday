library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(geomtextpath)
library(ggborderline)
library(showtext)
library(ggthemes)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

color.background = "#CFCFCF"
color.text ="black"

cagefree <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

cagefree <- cagefree %>% 
  mutate(year = year(observed_month),
         month = month(observed_month))
         
cagefree %>% 
  group_by(year) %>%
  summarise(total = mean(percent_hens)) %>% 
  ggplot(aes(year, total, fill = total))  +
  geom_line(color = "#D68A4F", linewidth = 5) +
  geom_point(fill = "#D68A4F",
             color = "black",
             size = 8,
             shape = 21) +
  geom_curve(aes(x = 2007, y = 3.2, xend = 2021, yend = 27.62), color = "black", curvature = 1.6, size = 1) +
  #geom_text(aes(label = round((total),2)), size = 10, fontface = "bold", family = "Roboto",vjust = 0.2,color = "white")+
  ylim(c(-20, 28)) +
  xlim(2007, 2025) +
  my_theme() +
  theme_economist()+
  theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Roboto'))+
  theme(legend.position = "none")+
  theme(axis.title.y = element_blank())
  
  # Fill the color, added title and subtitle using figma
         
         
