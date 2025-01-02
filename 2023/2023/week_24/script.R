library(tidyverse)
library(tidyr)
library(stringr)
library(paletteer)

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv')

df_split <- separate(safi_data, items_owned, into = paste0("column", 1:15), sep = ";")

df_split <- df_split |> 
  pivot_longer(cols = c(11:25),
               names_to = "items",
               values_to = "values")
               
df_split |> 
  filter(!is.na(values)) |>
  filter(values != 'NULL') |> 
  count(values) |> 
  arrange(desc(n)) |> 
  ggplot(aes(reorder(values,n),n, fill = n))+
  geom_bar(stat = "identity")+
  ylim(-80,100)+
  coord_polar()+
  scale_fill_paletteer_c("grDevices::Green-Yellow", direction = -1)+
  theme_void()+
  theme(legend.position = "none")+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.title.x = element_blank())
  
  # Added the icons and text, title uisng Figma
