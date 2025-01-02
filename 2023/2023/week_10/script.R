library(tidyverse)
library(scales)
library(MetBrewer)
library(lubridate)
library(janitor)
library(sf)
library(rnaturalearth)
library(ozmaps)

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

df <- numbats %>% 
  clean_names()
head(df)

oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")

# Map
ggplot()  + 
  geom_sf(data = oz_states, colour = "black", fill = "#adc178", alpha = 0.5) + 
  geom_point(data = df, mapping = aes(x = decimal_longitude, y = decimal_latitude), colour = "#fdc500") + 
  coord_sf()+
  theme_void()+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(axis.text.y = element_blank())+
theme(plot.title       = element_text(color="#FFFFFF", size=100, face = "bold"))+
theme(plot.subtitle    = element_text(color="#FFFFFF", size=50, face = "bold"))+
theme(plot.caption     = element_text(color="#FFFFFF", size=20, face = "bold", hjust = 0.5))+
theme(plot.background  = element_rect(fill="#292929", color="#292929"))


df$year <- ifelse(is.na(df$year), "unknown", df$year)

# Yealry Sightings
df %>% 
  count(year) %>% 
  ggplot(aes(year,n))+
  geom_point(size=15, color="#adc178")  +
  geom_segment(aes(x=year, xend=year, y=0, yend=n), color = '#adc178',size = 1) +
  geom_text(aes(label = n), size = 5, color = 'black')+
  theme_void()+
  theme(axis.text.x = element_text(angle = 45, color = "white"))+
  theme(plot.background  = element_rect(fill="#292929", color="#292929"))
  
# Donut Chart
df_name <- data.frame(
  group = c("Myrmecobius fasciatus", "Myrmecobius fasciatus rufus"),
  value = c(787, 18)
)

# Calculate the y positions for labels
df_name$label_y <- cumsum(df_name$value) - 0.5 * df_name$value

# Create a stacked bar chart
p <- ggplot(df_name, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("#adc178","#fdc500"))+
  theme_void()+
  theme(legend.position = "none")
  # Add a hole in the middle
p <- p + theme(panel.background = element_rect(fill = "#292929", colour = "#292929")) +
  theme(plot.background = element_rect(fill = "#292929", colour = "#292929")) +
  annotate("rect", xmin = -1, xmax = 1, ymin = -1, ymax = 1, fill = "#292929")

# Add labels
p <- p + geom_text(aes(y = label_y, label = group), color = "white",size = 10, nudge_x = 1.5)
# Print the plot
p

# Dashboard Created in Figma
