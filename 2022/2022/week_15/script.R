library(tidyverse)
library(janitor)
library(ggrepel)
library(scales)
library(countrycode)
library(hrbrthemes)
library(viridis)
library(paletteer)

indoor_pollution <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')
fuel_access <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_access.csv')
fuel_gdp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/fuel_gdp.csv')
death_source <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_source.csv')
death_full <-readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_full.csv')
death_time <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_timeseries.csv')

fuel_gdp_clean <- 
  fuel_gdp %>%
  clean_names() %>%
  rename(acess_clean_fuel = starts_with("access_to"),
         gdp_pc = starts_with("gdp_per"),
         pop = starts_with("population"),
         country = entity)

indoor_pollution_clean <- 
  indoor_pollution %>% 
  clean_names() %>% 
  rename(country = entity, 
         death = starts_with("deaths"))

fuel_indoor_pollution <- 
  fuel_gdp_clean %>% 
  left_join(indoor_pollution_clean)
  
my_theme1 <- function() {
  
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
    theme(legend.position = "top") +
    theme(legend.background = element_rect(fill=color.background, color=color.background))+
    theme(legend.title = element_blank())+
    theme(legend.text = element_text(size = 15, face = "bold", color=color.text))+

    
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold", hjust = 0.5))+
    theme(plot.subtitle =  element_text(color = color.text, size=15, face = "bold", hjust = 0.5))+
    theme(axis.title.x     = element_text(size=10, color = color.text, face = "bold")) +
    theme(axis.title.y     = element_text(size=10, color = color.text, face = "bold")) +
    theme(axis.text.x      = element_text(size=10, color = color.text, face = "bold")) +
    theme(axis.text.y      = element_text(size=10, color = color.text, face = "bold")) +
    theme(strip.text       = element_blank()) 
    
    # Plot margins
    #theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

data <- fuel_indoor_pollution %>% filter(year=="2010")%>%
  na.omit()
  
data %>%
  arrange(desc(gdp_pc)) %>%
  mutate(country = factor(country, country)) %>%
  ggplot(aes(x=death, y=acess_clean_fuel,size = gdp_pc, color=continent)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 24), name="GDP per capita")+
  guides(size = "none")+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  scale_colour_paletteer_d("khroma::light") +
  labs(title = "Access to Clean Fuels for Cooking VS Indoor Death Rate in 2010",
       subtitle = "Bubble size is GDP Per Capita",
       x = "Death Percent",
       y = "% of population having access to clean fuels for cooking") +
  my_theme1()  
