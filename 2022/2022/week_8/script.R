library(tidyverse)
library(dplyr)
library(hrbrthemes)
library(scales)
library(RColorBrewer)
library(viridis)

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

df <- freedom %>%
  janitor::clean_names() %>%
  rename(civil_liberties = cl,
         political_rights = pr)

df %>%
  count(year, sort = TRUE)%>%
  arrange(desc(year))

by_region <- df %>%
  filter(year == 2020) %>%
  group_by(region_name) %>%
  summarise(n_countries = n(),
            avg_civil_liberties = mean(civil_liberties),
            avg_political_rights = mean(political_rights),
            pct_free = mean(status == 'F'))

by_region %>%
  ggplot(aes(avg_civil_liberties, avg_political_rights))+
  geom_abline(color = 'red')+
  geom_point(aes(size = n_countries))+
  geom_text(aes(label = region_name), vjust =1 , hjust =1)+
  expand_limits(x = 0, y= 0, size =0)

df %>%
  filter(year == 2020) %>%
  gather(metric, value, civil_liberties, political_rights)%>%
  count(region_name, metric, value)%>%
  ggplot(aes(value, n))+
  geom_col()+
  facet_grid(region_name ~ metric)

my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
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
    theme(legend.position = "bottom") +
    theme(legend.background = element_rect(fill="#FFFFFF", 
                                           size=0.5, linetype="solid"))+
    
    
    
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

my.cols <- brewer.pal(5, "BrBG")
my.cols


df %>%
  gather(metric, value, civil_liberties, political_rights)%>%
  mutate(metric = str_to_title(str_replace_all(metric, "-"," ")),
         region_name = fct_reorder(region_name,value)) %>%
  group_by(year, region_name,metric)%>%
  summarise(avg_rating = mean(value))%>%
  ggplot(aes(year, avg_rating, color = region_name))+
  geom_line(size = 1)+
  geom_point(size = 1)+
  facet_wrap(~metric)+
  expand_limits(y = 0)+
  my_theme()+
  labs(x = "Year",
       y = "World Freedom Index Rating",
       title = "World Freedom Index Rating Over Time By Region",
       color = "Region")
