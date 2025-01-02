library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
library(patchwork)
library(streamgraph)
library(viridis)
library(hrbrthemes)
library(plotly)
library(RColorBrewer)


font_add_google("Open Sans","open")
font_add_google("Bebas Neue","bebas")
font_add_google("League Spartan","spart")
font_add_google("Fira Sans","fira")


babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

glimpse(babynames)

babynames %>%
  count(year, sort = TRUE)

babynames %>%
  count(sex, sort = TRUE)

babynames %>%
  filter(sex == "F")%>%
  count(name, sort = TRUE)

  
top20_f <- babynames%>%
  filter(sex=='F')%>%
  filter(year==2017)%>%
  arrange(-n)%>%
  head(20)


data <- babynames %>% 
  filter(name %in% c("Emma", "Olivia","Ava", "Isabella","Sophia","Mia","Charlotte","Amelia","Evelyn","Abigail",
                     "Harper","Emily","Elizabeth","Avery","Sofia","Ella","Madison","Scarlett","Victoria","Aria")) %>%
  filter(sex=="F")


my_theme <- function() {
  
  # Colors
  color.background = "#F5F5F5"
  color.text = "#22211d"
  
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
    theme(legend.position = "None") +
    theme(legend.background = element_blank())+
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



data %>%
  ggplot( aes(x=year, y=n, group=name, fill="#8B4789")) +
  geom_area() +
  theme(legend.position="none") +
  my_theme()+
  facet_wrap(~name, scale="free_y")+
  labs(x = " ",
       y = " ",
       title = "Popularity of American Female Names over the Years",
       subtitle = "Names : Top 20 names from year 2017")

  
