library(tidyverse)
library(ggsvg)

font_add(family = "3D", regular = "../input/montag/Montague.ttf")
font_add(family = "Rounded", regular = "../input/rounded/RockoFLF.ttf")
showtext_auto()

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

svg_url <- "https://www.svgrepo.com/download/89559/cable-car-cabin.svg"
svg_txt <- paste(readLines(svg_url), collapse = "\n")

color.background = "#FFF5EE"
color.text = "#030303"

library(repr)
options(repr.plot.width = 20, repr.plot.height =20)


p1 <- top_10 %>%
ggplot(aes(reorder(name, height),height))+
geom_point()+
geom_point_svg(aes(name, height),svg = svg_txt, size = 30) +
geom_segment(aes(x=name, xend=name, y=0, yend=height), color = 'black',size = 1) +
coord_polar()+
theme_minimal()+
theme(plot.background  = element_rect(fill="#EEDFCC", color="#EEDFCC"))+
theme(panel.grid.major = element_line(colour="gray"))+
theme(plot.title       = element_text(color=color.text, size=80, face = "bold", hjust = 0.5, family = '3D'))+
theme(plot.subtitle    = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Rounded'))+
theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Rounded'))+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.x      = element_text(size=25, color = "#8B3E2F", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Rounded')) +
theme(axis.text.y      = element_blank()) +
theme(strip.text       = element_text(size=20, color = "#8B3E2F", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Rounded'))+
theme(legend.position = "none")+
labs(title = "Ferris Wheels Around the World",
    subtitle = "Top 10 Ferris Wheels by height",
    caption = "Data Source : Ferris Wheels/TidyTuesday/week 31 2022")
p1
