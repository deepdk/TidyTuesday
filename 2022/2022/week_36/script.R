library(tidyverse)
library(scales)
library(lubridate)
library(janitor)
library(scales)
library(tidyr)
library(widyr)

font_add(family = "harry potter", regular = "../input/harry-potter-fonts/ParryHotter.ttf")
font_add(family = "magic school", regular = "../input/harry-potter-fonts/MagicSchoolTwo.ttf")
font_add_google('Fira Sans', 'firasans')
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
    theme(legend.position = "none") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+
    theme(legend.justification = "center")+
    theme(legend.title = element_text(family = "firasans",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
                                    
  # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
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

lego_datasets <- tibble(file = dir("../input/legodata", full.names = TRUE)) %>%
  mutate(data = map(file, read_csv)) %>%
  extract(file, "name", "legodata/(.*).csv") %>%
  deframe()
  
sets <- lego_datasets$sets 
head(sets)

sets_with_themes <- lego_datasets$sets %>%
  left_join(lego_datasets$themes %>%
              select(id, theme_name = name),
            by = c(theme_id = "id")) %>%
  mutate(num_parts = na_if(num_parts, 0))
  
by_theme <- sets_with_themes %>%
  group_by(theme_name) %>%
  summarize(n_sets = n(),
            median_parts = median(num_parts, na.rm = TRUE)) %>%
  arrange(desc(n_sets))  
  
set_parts <- sets_with_themes %>%
  inner_join(inventories_current, by = "set_num") %>%
  inner_join(lego_datasets$inventory_parts, by = "inventory_id", suffix = c("", "_inventory")) %>%
  left_join(lego_datasets$colors %>%
              rename(color = name), by = c(color_id = "id")) %>%
  mutate(rgb = paste0("#", rgb)) 
  
by_theme_color <- set_parts %>%
  count(theme_name,
        color = fct_lump(color, 20),
        rgb = fct_lump(rgb, 20),
        name,
        img_url,
        sort = TRUE) %>%
  filter(color != "Other")  
  
by_theme_color %>%
  filter(theme_name == "Harry Potter")%>%
  filter(name %in% c("Hogwarts Castle","Diagon Alley","Hogwarts Chamber Of Secrets","Hogwarts Great Hall","Hogwarts: Dumbledore’s Office",
                    "Hogsmeade Village Visit","The Ministry of Magic","The Forbidden Forest","Quidditch Match","The Knight Bus"))%>%
  ggplot(aes(n, name, fill = I(rgb))) +
  geom_col(position = "fill") +
  labs(x = "% of parts in this theme",
       y = "")+
my_theme()+
theme(plot.title       = element_text(color="#FFE4E1", size=150, face = "bold", hjust = 0.5, family = 'magic school'))+
    theme(plot.subtitle    = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'magic school'))+
    theme(plot.caption     = element_text(color=color.text, size=25, face = "bold", hjust = 0.5, family = 'firasans'))+
theme(axis.text.y     = element_text(size=40, color = "#FFE4E1", hjust = 0.5, vjust = -3.5,face = "bold", family = 'magic school'))+
theme(axis.text.x     = element_text(size=40, color = "#FFE4E1",face = "bold", family = 'firasans'))+
theme(axis.title.x     = element_text(size=25, color = "#FFE4E1",face = "bold", family = 'firasans'))+
labs(title = "Harry Potter LEGO Colours",   
     caption = "Data Source : LEGO data/TidyTuesday/week 36 2022 \n Graphic : Deepali Kank")
    
