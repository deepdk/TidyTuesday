library(tidyverse)
library(showtext)
library(MetBrewer)
library(png)
library(jpeg)
library(magick)
library(ggimage)

p_stat <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')

fri <- p_stat %>%
filter(uni_name == "Friends")

df1 <- fri %>%
select(-char_id, -uni_id, -uni_name, -question, -rank, -rating_sd, -number_ratings)%>%
filter(personality %in% c("expressive","emotional","opinionated","soft","awkward","bold","dramatic","playful","warm","spontaneous"))%>%
pivot_wider(names_from = personality, values_from = avg_rating,values_fn = list(avg_rating = mean))

df1$image <- c("../input/friends/monica.png","../input/friends/racheal.png","../input/friends/chandler.png",
             "../input/friends/Joey.png","../input/friends/phoebe.png","../input/friends/ross.png")
             
d1 <- df1 %>%
pivot_longer(cols = c("expressive","emotional","opinionated","soft","awkward","bold","dramatic","playful","warm","spontaneous"),
            names_to = 'personality',
            values_to = 'value')
            
p1 <- d1 %>%
ggplot(aes(x=as.factor(personality), y=value,  fill=personality)) +      
geom_bar(stat="identity")+
ylim(-50,100) +
geom_image(mapping=aes(y=-50,x=1,image=image), size=0.25)+
scale_fill_manual(values = met.brewer("Archambault",10))+
coord_polar()+
facet_wrap(~char_name)+
theme_light()+
theme(panel.background = element_rect(fill="black", color="black")) +
theme(plot.background  = element_rect(fill="black", color="black")) +
theme(panel.border     = element_rect(color="black")) +
theme(strip.background = element_rect(fill="black", color="black")) +
theme(strip.background = element_rect(color="black", fill="#FFBBFF", size=1, linetype="solid"))+
theme(strip.text.x = element_text(size = 30, color = "black", family = "firasans"))+
theme(axis.text.x = element_text(size = 20, color = "white", family = "firasans"))+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.y  = element_text(size=20, color = "#FFFFFF", family = 'firasans'))+
theme(plot.title   = element_text(color="#FFFFFF", size=60, face = "bold", hjust = 0.5, family = 'firasans'))+
theme(plot.subtitle = element_text(color="#FFFFFF", size=30,hjust = 0.5, family = 'firasans', face = "bold"))+
theme(legend.position = "none")

plot_file <- image_read("../input/personality/personality.png")
logo_file <- image_read("../input/filess/friendslogo.jpg")

final <- ggdraw() +
  draw_image(image = plot_file, x = 0, y = 0, width = 1, height = 1) +
  draw_image(image = logo_file, x = 0, y = -0.05, halign = 0.5, valign = 0.95, scale = 0.8)
final
