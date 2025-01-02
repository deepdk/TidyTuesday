library(tidyverse)
library(showtext)
library(patchwork)
library(ggtext)
library(lubridate)
library(janitor)
library(scales)
library(maps)
library(MetBrewer)

font_add(family = "Roboto",regular = "../input/roboto-bold/RobotoCondensed-Bold.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#363636"
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
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+

# Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, hjust = 0.5,face = "bold", family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df <- read.csv("../input/tidytuesday-week-38/hydrowaste.csv")

df <- clean_names(df)

df1 <- df %>%
mutate(qual_loc = as.factor(qual_loc),
      qual_pop = as.factor(qual_pop),
      qual_waste = as.factor(qual_waste),
      qual_level = as.factor(qual_level),
      coast_10km = as.factor(coast_10km),
      coast_50km = as.factor(coast_50km),
      qual_cap = as.factor(qual_cap))
      
world <- map_data("world")  

df2 <- df1 %>%
rename(region = country)%>%
group_by(region)%>%
summarise(total = sum(waste_dis))%>%
mutate(per = (total/sum(total)))

df2 <- df2 %>%
  ## Recode certain entries
  mutate(region = recode(str_trim(region), "United States" = "USA",
                            "United Kingdom (Montserrat)" = "UK"))
                            
final = inner_join(world, df1_top10, by = "region")

p1 <- ggplot(data = final, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = per), color = "gray25") +
  scale_fill_gradientn(colors = met.brewer("Morgenstern"), labels = scales::percent) + 
  my_theme()+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(legend.key.size = unit(1, 'cm'))+
  labs(title = "Percent of Treated wastewater discharged by WWTPs")
  
p2 <- df1 %>% 
filter(coast_10km == 1)%>%
count(country, sort = TRUE)%>%
arrange(desc(n))%>%
head(20)%>%
ggplot(aes(fct_reorder(country,n),n, fill = fct_reorder(country,n)))+
geom_bar(stat = "identity")+
geom_text(aes(label = n), size = 10, color = 'black', hjust = 1.5, fontface = "bold")+
coord_flip()+
scale_fill_manual(values = met.brewer("Morgenstern", 20))+
my_theme()+
theme(axis.text.x = element_blank())+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
labs(title = "Number of WWTPs where estimated waste discharge \n outfall location within 10 km of the ocean or a large lake",
    caption = "Data Source : Waste Water Plants/Tidytuesday week 38 2022 \n Graphic : Deepali Kank")
    
p2 + annotation_custom(ggplotGrob(p1), xmin = 1, xmax = 10.5, 
                       ymin = 400, ymax = 1200)+
plot_annotation(title = "Waste Water Plants",
                    theme = theme(plot.title = element_text(size = 90, face = "bold",color = "#FFFFFF",hjust = 0.5, family = "Roboto"),
                                  plot.background  = element_rect(fill="#363636", color="#363636")))    
