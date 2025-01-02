library(tidyverse)
library(RColorBrewer)

airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

glimpse(airmen)

airmen %>%
  count(graduated_from, sort = TRUE)
 
airmen %>%
  count(reported_lost, sort = TRUE)
 
airmen %>%
  count(rank_at_graduation, sort = TRUE)


airmen %>%
  count(state, sort = TRUE)



airmen$pilot_type <- gsub("Liason pilot","Liaison pilot",airmen$pilot_type)


states_abbreviation <- read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv")

airmen <- airmen %>%
  left_join(states_abbreviation, by = c("state" = "Abbreviation")) 

my_theme <- function() {
  
  # Colors
  color.background = "#FFFACD"
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
    theme(legend.background = element_rect(fill="#FFFACD", 
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


my.cols <- brewer.pal(4, "BrBG")
my.cols

airmen %>%
  drop_na(State)%>%
  ggplot(aes(y = forcats::fct_rev(forcats::fct_infreq(State)),fill = pilot_type)) +
  geom_bar() +
  scale_fill_manual(values = my.cols)+
  my_theme()+
  labs(x = "Count",
       y = "State",
       title = "African American Pilots by State")
 
