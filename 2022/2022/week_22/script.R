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
library(janitor)
library(ggbump)

library(repr)
options(repr.plot.width = 25, repr.plot.height =30)

font_add_google('Fira Sans', 'firasans')
font_add(family = "Serif", regular = "../input/libre-font/LibreBaskerville-Bold.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#FFFACD"
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
    theme(legend.text = element_text(size = 25, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",
                                    color = "#FFFFFF",
                                    size = 25, face = "bold"))+
                                    
   # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Serif'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'Serif'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Serif'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Serif')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Serif')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Serif')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Serif')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Serif')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')

poll <- poll %>%
clean_names()

df <- poll %>% 
select(-rq)%>%
pivot_wider(names_from = year, values_from = rank)
df

tech <- df %>%
filter(industry == 'Tech')
tech

p1 <- tech %>%
ggplot(aes(reorder(company, x2022_rank), x2022_rank)) +
  geom_point( size=15, color="#8B0A50", fill=alpha("#97FFFF", 0.3), alpha=0.7, shape=21, stroke=2)  +
  geom_segment(aes(x=company, xend=company, y=0, yend=x2022_rank), color = '#8B0A50',size = 1) +
  geom_text(aes(label = x2022_rank), size = 10)+
  coord_flip()+
my_theme()+
labs(title = 'Tech')+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.x = element_blank())
p1

retail <- df %>%
filter(industry == 'Retail')

p2 <- retail %>%
ggplot(aes(reorder(company, x2022_rank), x2022_rank)) +
  geom_point( size=15, color="#8B0A50", fill=alpha("#97FFFF", 0.3), alpha=0.7, shape=21, stroke=2)  +
  geom_segment(aes(x=company, xend=company, y=0, yend=x2022_rank), color = '#8B0A50',size = 1) +
  geom_text(aes(label = x2022_rank), size = 10)+
  coord_flip()+
my_theme()+
labs(title = 'Retail')+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.x = element_blank())
p2

f1_df <- df %>%
filter(industry == 'Food & Beverage')

p3 <- f1_df %>%
ggplot(aes(reorder(company, x2022_rank), x2022_rank)) +
  geom_point( size=15, color="#8B0A50", fill=alpha("#97FFFF", 0.3), alpha=0.7, shape=21, stroke=2)  +
  geom_segment(aes(x=company, xend=company, y=0, yend=x2022_rank), color = '#8B0A50',size = 1) +
  geom_text(aes(label = x2022_rank), size = 10)+
  coord_flip()+
my_theme()+
labs(title = 'Food and Beverages')+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.x = element_blank())
p3

fins <- df %>%
filter(industry == 'Financial Services')

p4 <- fins %>%
ggplot(aes(reorder(company, x2022_rank), x2022_rank)) +
  geom_point( size=15, color="#8B0A50", fill=alpha("#97FFFF", 0.3), alpha=0.7, shape=21, stroke=2)  +
  geom_segment(aes(x=company, xend=company, y=0, yend=x2022_rank), color = '#8B0A50',size = 1) +
  geom_text(aes(label = x2022_rank), size = 10)+
  coord_flip()+
my_theme()+
labs(title = 'Financial Services')+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(axis.text.x = element_blank())
p4

P = (p1+p2)/(p3+p4)

P + plot_annotation(title = 'Rank Of Companies in 2022 According To Axios Harris Poll',
                    subtitle = 'Industries = Tech, Retail, Food and Beverages, Financial Services  \n Lower Rank is best',
                    theme = theme(plot.title = element_text(size = 50, face = "bold", hjust = 0.5, color = "#030303", family = 'Serif'),
                                  plot.subtitle = element_text(size = 40, face = "bold", hjust = 0.5, color = "#030303", family = 'Serif'),
                                  plot.caption = element_text(size = 25, face = "bold", hjust = 0.5, color = "#FFFFFF", family = 'Serif'),
                                  plot.background  = element_rect(fill="#FFFACD", color="#FFFACD")))
    
