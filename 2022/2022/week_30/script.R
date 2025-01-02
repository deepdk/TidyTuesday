library(tidyverse)
library(MetBrewer)
library(ggimage)
library(png)

font_add(family = "Serif", regular = "../input/bullpen/Bullpen3D.ttf")
font_add(family = "Horror", regular = "../input/horror/mrsmonster.ttf")
font_add(family = "Rounded", regular = "../input/horror/RockoFLF.ttf")
showtext_auto()

# Reading the datafile
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv')

# Cleaning and Wrangling
df1 <- df %>% pivot_longer(cols=c('caught_fred', 'caught_daphnie', 'caught_velma','caught_shaggy','caught_scooby',
                                'captured_fred', 'captured_daphnie', 'captured_velma','captured_shaggy','captured_scooby',
                                'unmask_fred','unmask_daphnie','unmask_velma','unmask_shaggy','unmask_scooby',
                                'snack_fred','snack_daphnie','snack_velma','snack_shaggy','snack_scooby'),
                    names_to='name',
                    values_to='value')
                    
fred <- df1 %>%
filter(name %in% c('caught_fred', 'captured_fred', 'unmask_fred','snack_fred'))%>%
filter(value == TRUE)%>%
count(name, sort = TRUE)
fred

daphnie <- df1 %>%
filter(name %in% c('caught_daphnie', 'captured_daphnie', 'unmask_daphnie','snack_daphnie'))%>%
filter(value == TRUE)%>%
count(name, sort = TRUE)
daphnie

velma <- df1 %>%
filter(name %in% c('caught_velma', 'captured_velma', 'unmask_velma','snack_velma'))%>%
filter(value == TRUE)%>%
count(name, sort = TRUE)
velma

shaggy <- df1 %>%
filter(name %in% c('caught_shaggy', 'captured_shaggy', 'unmask_shaggy','snack_shaggy'))%>%
filter(value == TRUE)%>%
count(name, sort = TRUE)
shaggy

scooby <- df1 %>%
filter(name %in% c('caught_scooby', 'captured_scooby', 'unmask_scooby','snack_scooby'))%>%
filter(value == TRUE)%>%
count(name, sort = TRUE)
scooby

# Creating a new dataframe with images
name <- c("Fred","Daphnie","Velma","Shaggy","Scooby")
captured <- c(71, 91, 74, 85, 83)
caught <- c(132, 29, 41, 77, 160)
unmask <- c(102, 37, 94,13,23)
ate_snack <- c(18, 49, 29, 13, 23)
image <- c("../input/chara-images/Fred.png","../input/chara-images/Dephnie.png","../input/chara-images/Velma.png","../input/chara-images/Shaggy.png","../input/chara-images/scooby.png")

dataframe <- data.frame(name, captured, caught, unmask, ate_snack, image)
dataframe

d1 <- dataframe %>%
pivot_longer(cols = c('captured','caught','unmask','ate_snack'),
            names_to = 'work',
            values_to = 'value')
            
my_image <- readPNG("../input/scooby-do-and-friends/Scooby_Doo_and_Friends_Transparent_PNG_Clip_Art_Image.png", native = TRUE)

p1 <- d1 %>%
ggplot(aes(x=as.factor(work), y=value,  fill=work)) +      
geom_bar(stat="identity")+
ylim(-50,160) +
geom_image(mapping=aes(y=-50,x=1,image=image), size=0.135)+
scale_fill_manual(values = c("#FF7F00","#458B00","#68228B","#8B1A1A"))+
coord_polar()+
facet_wrap(~name)+
theme_light()+
theme(strip.background = element_rect(color="black", fill="#87CEFA", size=1, linetype="solid"))+
theme(strip.text.x = element_text(size = 30, color = "black", family = "Rounded", face = "bold"))+
theme(axis.text.x = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.y  = element_text(size=20, color = "#030303", family = 'firasans'))+
theme(plot.title   = element_text(color="#030303", size=100, face = "bold", hjust = 0.5, family = 'Horror'))+
theme(plot.subtitle = element_text(color="#030303", size=30,hjust = 0.5, family = 'Rounded', face = "bold"))+
theme(plot.caption  = element_text(color="#030303", size=20, hjust = 0.5, family = 'Rounded'))+
theme(legend.position = "top") +
    theme(legend.background = element_rect(fill="#FFFFFF", color="#FFFFFF"))+
    theme(legend.text = element_text(size = 30, color="#030303", family = "Horror"))+
    theme(legend.justification = "center")+
    theme(legend.title = element_blank())+
    theme(legend.key.size = unit(1, 'cm'))+
    labs(title = "Scooby Doo Where are You??",
    subtitle = "Scoobypedia is an encyclopedia on the hit television series Scooby-Doo which has been airing for over 50 years!\nThe show follows the iconic mystery solving detectives, know as Mystery Inc.,as they set out to solve crime and unmask criminals, \n  bent on revenge or committing criminal acts for their own personal gain.",
    caption = "Data Source : ScoobyPedia/TidyTuesday/Week 29 2021")+
inset_element(p = my_image,
                left = 0.68,
                bottom = 0,
                right = 0.99,
                top = 0.50)
p1
