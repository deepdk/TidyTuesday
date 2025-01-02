library(tidyverse)
library(paletteer)
library(showtext)
library(tidytext)
library(reshape2)
library(ggrepel)
library(ggtext)
library(lubridate)
library(janitor)
library(geofacet)
library(patchwork)

# Set Font
font_add_google('Fira Sans', 'firasans')
showtext_auto()


Set Theme
my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text = "#FFFFFF"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "right") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 20, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",color = "#030303",size = 10, face = "bold"))+
    theme(legend.key.size = unit(3, 'cm'))+
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    
     # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
    }

# Read Data
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')
us_states <- map_data('state')

state <- us_states %>%
mutate(region = str_to_title(region))%>%
select(region, long, lat, group)

drought_fips <- drought_fips %>%
separate(date, c('year','month','day'))

grouped <- drought_fips %>%
group_by(State, year)%>%
summarise(avg = mean(DSCI))%>%
ungroup()

main_2011 <- grouped %>%
filter(year == 2011)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2011 <- main_2011 %>%
left_join(state, by = c('State'='region'))

p1 <- final_2011 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
  labs(title = "2011")+
  my_theme()
p1

main_2012 <- grouped %>%
filter(year == 2012)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2012 <- main_2012 %>%
left_join(state, by = c('State'='region'))


p2 <- final_2012 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2012")+
my_theme()
p2

main_2013 <- grouped %>%
filter(year == 2013)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2013 <- main_2013 %>%
left_join(state, by = c('State'='region'))


p3 <- final_2013 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2013")+
my_theme()
p3

main_2014 <- grouped %>%
filter(year == 2014)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2014 <- main_2014 %>%
left_join(state, by = c('State'='region'))


p4 <- final_2014 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2014")+
my_theme()
p4


main_2015 <- grouped %>%
filter(year == 2015)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2015 <- main_2015 %>%
left_join(state, by = c('State'='region'))


p5 <- final_2015 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2015")+
my_theme()
p5

main_2016 <- grouped %>%
filter(year == 2016)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2016 <- main_2016 %>%
left_join(state, by = c('State'='region'))


p6 <- final_2016 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2016")+
my_theme()
p6

main_2017 <- grouped %>%
filter(year == 2017)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2017 <- main_2017 %>%
left_join(state, by = c('State'='region'))


p7 <- final_2017 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
labs(title = "2017")+
my_theme()
p7


main_2018 <- grouped %>%
filter(year == 2018)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2018 <- main_2018 %>%
left_join(state, by = c('State'='region'))


p8 <- final_2018 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
 scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
my_theme()+
labs(title = "2018")
p8

main_2019 <- grouped %>%
filter(year == 2019)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2019 <- main_2019 %>%
left_join(state, by = c('State'='region'))


p9 <- final_2019 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
my_theme()+
labs(title = "2019")
p9

main_2020 <- grouped %>%
filter(year == 2020)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2020 <- main_2020 %>%
left_join(state, by = c('State'='region'))


p10 <- final_2020 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
my_theme()+
labs(title = "2020")
p10

main_2021 <- grouped %>%
filter(year == 2021)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2021 <- main_2021 %>%
left_join(state, by = c('State'='region'))


p11 <- final_2021 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
my_theme()+
labs(title = "2021")
p11

main_2022 <- grouped %>%
filter(year == 2022)%>%
mutate(State = state.name[match(State,state.abb)])%>%
na.omit()

final_2022 <- main_2022 %>%
left_join(state, by = c('State'='region'))


p12 <- final_2022 %>%
  ggplot(aes(x=long,y=lat,group=group, fill=avg)) +
  geom_polygon(color = "black", size = 0.1) +
  #coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  #coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_paletteer_c("pals::coolwarm",breaks = c(0, 100,200,300,400,500), limits = c(0,500))+
my_theme()+
labs(title = "2022")
p12


P = (p1+p2)/(p3+p4)/(p5+p6)/(p7+p8)/(p9+p10)/(p11+p12)

P + plot_annotation(title = 'US Drought Conditions',
                    subtitle = "Average Drought Score for Year 2011-2022 \n  0 : None of the area is abnormanlly dry or in drought\n  500 : The area is in D4, exceptional drought",
                    caption = "Data Source : US Drought/TidyTuesday Week 24-2022",
                    theme = theme(plot.title = element_text(size = 50, face = "bold", hjust = 0.5, color = "#FFFFFF", family = 'firasans'),
                                  plot.subtitle = element_text(size = 25, face = "bold", hjust = 0.5, color = "#FFFFFF", family = 'firasans'),
                                  plot.caption = element_text(size = 25, face = "bold", hjust = 0.5, color = "#FFFFFF", family = 'firasans'),
                                  plot.background  = element_rect(fill="#030303", color="#030303"),
                                  legend.position = "right"))+
plot_layout(guides = "collect", widths = 5, heights = 2)


  

