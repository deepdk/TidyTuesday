library(tidyverse)
library(paletteer)
library(showtext)
library(reshape2)
library(ggtext)
library(lubridate)
library(janitor)
library(scales)
library(ggridges)

font_add_google('Fira Sans', 'firasans')
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text = "#FFF5EE"
  
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
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=60, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

rent <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-07-05/rent.csv')

rent %>%
filter(county == "san francisco")%>%
filter(beds %in% c(1,2,3))%>%
mutate(beds = as.factor(beds))%>%
ggplot(aes(year, price, color = beds))+
geom_jitter()+
scale_color_paletteer_d("awtools::spalette")+
scale_x_discrete(limits = c(2000:2018) )+
my_theme()+
labs(title = "San Francisco County Rent Prices", 
     subtitle = "Bay Area Craigslist Rental Housing Posts, 2000-2018.",
     caption = "Data Source : Kate Pennington, data.sfgov.org, Vital Signs/TidyTuesday/Week 27 2022.",
    color = "Number of Bedrooms")+
theme(legend.position = "top") +
theme(legend.key = element_rect(fill = "#030303"))+
theme(legend.text = element_text(size = 40, face = "bold", color = 'white'))+
theme(legend.justification = "center")+
theme(legend.title = element_text(family = "firasans",color = "white",size = 40, face = "bold"))+
theme(legend.key.size = unit(2, 'cm'))+
theme(legend.background = element_rect(fill = "transparent"))+
guides(color = guide_legend(override.aes = list(size = 10)))


rent %>%
filter(!is.na(county))%>%
ggplot(aes(price,county))+
geom_density_ridges2(fill = "#FFE1FF")+
scale_x_log10()+
my_theme()+
theme(legend.position = "none")+
labs(title = "Rent In SF According to County",
    caption = "Data Source : Kate Pennington, data.sfgov.org, Vital Signs/TidyTuesday/Week 27 2022.")
