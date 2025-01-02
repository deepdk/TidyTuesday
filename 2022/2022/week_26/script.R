library(tidyverse)
library(paletteer)
library(showtext)
library(tidytext)
library(reshape2)
library(ggrepel)
library(ggtext)
library(lubridate)
library(janitor)
library(scales)
library(tidyr)

font_add_google('Fira Sans', 'firasans')
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
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
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
# Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold",family = 'firasans'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", family = 'firasans'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'firasans'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'firasans')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

paygap <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/paygap.csv')

df <- paygap %>%
mutate(date_submitted = year(date_submitted))

df1<- df %>%
filter(date_submitted == 2022,
      employer_size == '20,000 or more')
      
df2 <- df1[!duplicated(df1$employer_name), ]

df3 <- df2 %>%
select(employer_name, diff_mean_bonus_percent)%>%
arrange(desc(diff_mean_bonus_percent))%>%
filter(employer_name != "GREGGS PLC")

color <- ifelse(df3$diff_mean_bonus_percent < 0, "#8B3E2F", "#556B2F")

ggplot(df3, aes(x = reorder(employer_name, diff_mean_bonus_percent), y = diff_mean_bonus_percent)) +
  geom_bar(stat = "identity",
           show.legend = FALSE,
          fill = color,
          color = 'black') +
  geom_curve(x = 10, y = -30,
             xend = 4, yend = -35,
             color = 'black',
             arrow = arrow())+
  annotate("text", x = 12,y = -20, label = " \n negative = women's \n mean bonus \n pay is higher", color = "black", 
         size = 8, fontface = "bold", family = 'firasans') +
  coord_flip() +
  my_theme()+
  labs(y = "Diff_Mean_Bonus_Percent",
       title = "UK Pay Gap (2022)",
      subtitle = "Difference Between Percent Mean Bonus of Men And Women \n Size of the Company is 200000 or more",
      caption = "Data Source : UK PayGap/TidyTuesday/Week 26 2022")+
  theme(axis.title.y = element_blank())           
