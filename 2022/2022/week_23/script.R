library(tidyverse)
library(paletteer)
library(showtext)
library(tidytext)
library(reshape2)
library(ggrepel)
library(ggtext)
library(lubridate)
library(janitor)

font_add_google('Fira Sans', 'firasans')
font_add(family = "Sans Serif", regular = "../input/tabara/Tabarra-Shadow-Italic-FFP.otf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#FFF5EE"
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
    theme(legend.position = "none") +
    
   # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Sans Serif'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    
     # Plot margins
     theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')

df <- pride_aggregates %>%
subset(company != "Grand Total")%>%
mutate(company = fct_reorder(company, total_contributed))
df

p1 <- ggplot(df, aes(total_contributed, company,fill = company))+
geom_col(width = 1.05)+
scale_x_continuous(limits = c(0,700000),
                  labels = scales::dollar)+
geom_label(aes(label = company,x=0),size = 5, fontface = 'bold', vjust = 0, hjust = 0)+
scale_fill_paletteer_d("khroma::smooth_rainbow")+
scale_color_paletteer_d("khroma::smooth_rainbow")+
annotate("text", x = 350000, y = 2, label = "Pride Sponsers Who Have Donated To Anti-LGBTQ Campaigns ", size = 15, family = 'firasans', fontface = 'bold')+
annotate("text", x = 350000, y = 10, label = "    \n    Each year, hundreds of corporations around the country participate in Pride,   \n  an annual celebration of the LGBTQ+ community’s history and progress.     \n    They present themselves as LGBTQ+ allies,          \n        but new research from Data for Progress finds        \n   that in between their yearly parade appearances,           \n         dozens of these corporations are giving to state politicians        \n      behind some of the most bigoted and harmful policies in over a decade.", size = 10, family = 'firasans', fontface = 'bold')+
my_theme()+
labs(title = "ACCOUNTABLE ALLIES: HOLDING CORPORATIONS ACCOUNTABLE AT PRIDE",
    caption = "Data Source : Pride Donations/TidyTuesday/Week 23-2022")+
theme(legend.position = 'none')+
theme(axis.text.y = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
coord_polar(clip = "off", direction = -1)
p1

