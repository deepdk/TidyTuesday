library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(lubridate)
library(showtext)

font_add_google("Roboto","roboto")
showtext_auto()

nhl_player_births <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_player_births.csv')

nhl_player_births |> 
  mutate(day = weekdays(birth_date)) |> 
  mutate(day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) |> 
  mutate(birth_month = as.factor(birth_month)) |> 
  group_by(birth_month, day) |> 
  count(birth_month) |> 
  ggplot(aes(birth_month, day, color = n)) + 
  geom_point(aes(size = n)) + 
  scale_size(range = c(1, 45)) +
  scale_color_gradientn(colors = c(low = "#f6cacc",high = "#dd2c2f")) +
  geom_text(aes(label = n), color = "black", size = 10) +
  theme_minimal() + 
  theme(legend.position = "none")  +
  theme(axis.text.x = element_text(size=30, color = "black", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
 theme(axis.text.y = element_text(size=30, color = "black", face = "bold", family = 'Roboto')) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank())

# Added the Ball images in Figma
