library(tidyverse)
library(reshape2)
library(lubridate)
library(janitor)
library(scales)
library(qdap)

font_add_google('Fira Sans', 'firasans')
font_add(family = "Sans Serif", regular = "../input/sans-serif/SansSerifBldFLF.otf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#d2b48c"
  color.text = "#030303"
  
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
    theme(legend.text = element_text(size = 20, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",color = "#030303",size = 20, face = "bold"))+
    theme(legend.key.size = unit(1, 'cm'))+
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=50, face = "bold",hjust = 0.5,  family = 'Sans Serif'))+
    theme(plot.subtitle    = element_text(color=color.text, size=40, face = "bold",hjust = 0.5, family = 'Sans Serif'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans', angle = 90)) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

african_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv')

african_names %>%
  replace_na(list(gender = 'Unknown'))%>%
  mutate(gender = factor(gender, levels = c("Boy", "Girl", "Man","Woman","Unknown")))%>%
  mutate(year_arrival=as.character(year_arrival))%>%
  arrange(year_arrival)%>%
  ggplot(aes(y = year_arrival,fill = gender)) +
  geom_bar() +
  scale_fill_manual(values = c('#dc143c','#00aa00','#4682b4','#ffd700','#654321'))+
  coord_flip()+
  my_theme()+
  labs(title = "History of Slavery in United States",
      subtitle = "Record of slaves who were freed during their forced transport.")
