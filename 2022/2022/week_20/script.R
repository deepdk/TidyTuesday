devtools::install_github("rensa/ggflags", force = TRUE)

library(tidyverse)
library(paletteer)
library(showtext)
library(patchwork)
library(naniar)
library(tidytext)
library(reshape2)
library(ggrepel)
library(ggtext)
library(lubridate)
library(ggbeeswarm)
library(glue)
library(ggflags)
library(scales)

library(repr)
options(repr.plot.width = 20.0, repr.plot.height = 20)

font_add(family = "Serif", regular = "../input/bullpen/Bullpen3D.ttf")
font_add_google(family = "Roboto", name = "Roboto")
font_add_google(family = "Outfit", name = "Outfit")
font_add_google('Fira Sans', 'firasans')
showtext_auto()

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
    theme(legend.position = "top") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 25, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",
                                    color = "#FFFFFF",
                                    size = 25, face = "bold"))+
                                    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.title.y     = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.x      = element_text(size=17, color = color.text,angle = 90, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=17, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(size=17, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')
c_codes <- read.csv("../input/iso-country-codes-global/wikipedia-iso-country-codes.csv")

c_codes <- rename(c_codes,artist_country=English.short.name.lower.case)

euro_points1 <- eurovision %>%
    group_by(year) %>%
    slice(which.max(total_points)) %>%
    mutate(winner = paste(artist_country, "(", total_points, "Points )"))
    
df1 <- left_join( euro_points1, c_codes,by = 'artist_country')
df1 <- rename(df1,country=Alpha.2.code)

winners <- df1 %>%
filter(year >= 1980 & year != 2020)%>%
mutate(country = str_to_lower(country),
        country = as.character(country))
        
p1 <- ggplot(winners, aes(year, total_points))+
geom_bar(stat = 'identity',fill = '#363636', width = 0.04) +
scale_x_discrete(limits = c(1980:2022))+
scale_fill_paletteer_d("beyonce::X93")+
geom_flag(aes(country = country), size = 12)+
geom_text(aes(
        label = artist),
               size = 7,angle = 90, hjust = 1.5, vjust = 0.5, color = 'white', family = 'Outfit', fontface = 'bold',
    data = winners
    )+
  annotate("text", x=1997, y=650, label= "Eurovision Contest Winners", color = 'white', size = 30, family = 'Serif') +
  annotate("text", x=1997, y=600, label= "From 1980-2022", color = 'white', size = 20, family = 'Serif')+
my_theme()+
labs( x = 'Year',
     y = 'Points')
p1       
    


