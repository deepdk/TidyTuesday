library(tidyverse)
library(showtext)
library(sf)

font_add(family = "Brush",regular = "D:/HP laptop/Fonts/Admiration Pains.ttf")
font_add(family = "roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Regular.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
  color.text = "#363636"
    
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
    theme(legend.text = element_text(size = 10, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "Roboto",color = "white",size = 10, face = "bold"))+
    theme(legend.key.size = unit(1, 'cm'))+
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=30, hjust = 0.5,face = "bold", family = 'Brush'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, hjust = 0.5,face = "bold", family = 'Brush'))+
    theme(plot.caption     = element_text(color=color.text, size=10, face = "bold", hjust = 0.5, family = 'roboto'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Brush')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Brush')) +
    theme(axis.text.x      = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Brush')) +
    theme(axis.text.y      = element_text(size=20, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Brush')) +

# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

england = rnaturalearth::ne_countries(country = 'united kingdom', type='map_units',scale = 'medium',returnclass = "sf") %>% 
  filter(geounit=="England") 
england2 = st_transform(england, 7405)

df1 = df %>% filter(Governance != "University" & Governance != "Unknown") %>% 
  filter(str_detect(Admin_area,"England")) %>%
  select(Latitude,Longitude,Governance) %>%
  separate(Governance, c("type1","tyoe2"), "-") %>%
  mutate(type1=str_trim(str_replace_all(typ1,"_"," ")))
head(df1)

df2 = sf_project(from = st_crs(4326), to = st_crs(7405), df1[, c("Longitude", "Latitude")]) |> as.data.frame()
df3 = cbind(df1,df2)

df3 %>% ggplot() +
  geom_sf(data=england2) +
  geom_point(aes(V1,V2, color = subject1),alpha = 0.5) +
  facet_wrap(~subject1)+
  coord_sf(datum = NA)+
  scale_color_manual(values = c("#8B1C62","#6B8E23"))+
  my_theme()+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(legend.position = "none")+
  labs(title = "Museums in England",
       caption = "Data Source : UK Museums/TidyTuesday/Week 47 2022 \n Graphic : Deepali Kank")
