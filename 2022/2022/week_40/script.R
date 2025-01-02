library(tidyverse)
library(showtext)
library(ggtext)
library(lubridate)
library(ggthemes)
library(stringr)
library(MetBrewer)
library(ggimage)
library(cropcircles)

font_add(family = "Roboto", regular = "../input/roboto-bold/RobotoCondensed-Bold.ttf")
showtext_auto()

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

most_upvotes <- df %>%
mutate(year = as.factor(year))%>%
group_by(year) %>%
slice(which.max(upvotes)) %>%
mutate(name_new = paste(name, "(", upvotes, "Upvotes )"))%>%
mutate(images_cropped = circle_crop(main_image))

p1 <- ggplot(data = df, aes(year, upvotes)) +
geom_jitter(aes(color = year), width = 0.25, alpha = 0.2, size = 0.85) +
scale_color_manual(values = met.brewer("Lakota",8))+
geom_point(data = most_upvotes, aes(year, upvotes),size=10) +
geom_segment(aes(x=year, xend=year, y=0, yend=upvotes), color = '#696969',size = 0.5)+ 
geom_image(data = most_upvotes, mapping=aes(y=upvotes, x=year, image=images_cropped),  color="#696969", size=0.059, asp=1.5)+
geom_image(data = most_upvotes, mapping=aes(y=upvotes, x=year, image=images_cropped), size=0.05, asp=1.5) + 
geom_text(data = most_upvotes, aes(label = name), size = 10,hjust = 2, vjust = -1.,color = '#CD0000', fontface = "bold",family = 'Roboto')+
coord_flip()+
theme_fivethirtyeight()+
theme(legend.position = "none")+
theme(plot.title       = element_text(size=80, face = "bold", family = 'Roboto'))+
    theme(plot.subtitle    = element_text(size=40, face = "bold", family = 'Roboto'))+
    theme(plot.caption     = element_text( size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_text(size=20, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, face = "bold", family = 'Roboto'))+
labs(title = "Most Upvoted Product of the Year",
     subtitle = "Dataset of 76,000+ tech products on Product Hunt, \n a popular social network for launching and promoting such things",
    caption = "Data Source : Product Hunt Products/Tidytuesday/Week 40 2022 \n Graphic : Deepali Kank")
p1

