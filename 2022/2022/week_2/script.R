library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(magrittr)
library(broom)
library(rgeos)

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

glimpse(colony)

# Mean percentage loss over the years

colony_loss_year <- colony %>%
  group_by(year) %>% 
  summarize(lost_pct = mean(colony_lost_pct, na.rm = TRUE))

mean_loss <- ggplot() +
  geom_col(data = colony_loss_year, aes( x = year, y = lost_pct),width=.3,fill = '#e5ac3f') +
  ggtitle( "Mean Colony Loss Percentage Over the Years in US" ) +
  coord_flip()

# Bee Colony Percentage Loss in the year 2020

colony_2020 <- colony %>% 
  filter(year == 2020)%>%
  select(state, colony_lost_pct)

spdf <- geojson_read("../input/states/us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

df <- spdf_fortified %>% 
  left_join(., colony_2020, by = c("id" = "state"))


library(viridis)


bee1_change <- ggplot() +
  geom_polygon(data = df, aes(fill = colony_lost_pct, x = long, y = lat, group = group),size=0, alpha=0.9,) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white", size=3, alpha=0.6) +
  theme_void()+
  ggtitle( " Bee Colony Lost Percentage in the year 2020" ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

bee1_change  + scale_fill_viridis_c(option = "plasma")
