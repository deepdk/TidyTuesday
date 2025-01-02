library(tidyverse)
library(scales)
library(janitor)
library(lubridate)
library(dplyr)
library(showtext)
library(ggbrick)
library(waffle)

f = font_add_google("Nosifer","nosifer")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text ="#F2F2F2"
  
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
    theme(legend.title = element_text(family = "Roboto",
                                    color = "#030303",
                                    size = 15, face = "bold"))+
# Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'nosifer'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'nosifer'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'nosifer'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'nosifer')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'nosifer')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'nosifer')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'nosifer')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'nosifer')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-06-04/cheeses.csv')

# Update country names
cheeses <- cheeses %>%
  mutate(country = ifelse(country == "England, Great Britain, United Kingdom", "England", country)) |> 
  mutate(country = ifelse(country == "United Kingdom","England", country))

# Filter relevant countries
cheeses <- cheeses %>%
  filter(country %in% c("United States", "France", "Italy", "Canada", "Australia",
                        "England", "Ireland", "Germany", "Netherlands","Spain"))

# Calculate count per color and country
country_colors <- cheeses %>%
  group_by(country, color) %>%
  count(name = "n")

# Calculate total count per country to order the facets
country_totals <- country_colors %>%
  group_by(country) %>%
  summarize(total = sum(n)) %>%
  arrange(desc(total))

# Join totals back to the main data to reorder country levels
country_colors <- country_colors %>%
  left_join(country_totals, by = "country") %>%
  mutate(country = factor(country, levels = country_totals$country))

# Create the plot
ggplot(country_colors, aes(fill = color, values = n)) +
  geom_waffle(color = "black", n_rows = 5, flip = TRUE,size = 0.1,
    radius = unit(15, "pt")) +
  facet_wrap(~country, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(labels = function(x) x * 5, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values = c("blue" = "#0000FF", "blue-grey" = "#6699CC", "brown" = "#A52A2A", "brownish yellow" = "#CC9966",
                               "cream" = "#FFFFCC", "golden orange" = "#FFCC00", "golden yellow" = "#FFD700", "green" = "#008000",
                               "ivory" = "#FFFFF0", "orange" = "#FFA500", "pale white" = "#FAFAFA", "pale yellow" = "#FFFF99",
                               "pink and white" = "#FFC0CB", "red" = "#FF0000", "straw" = "#E4D96F", "white" = "#FFFFFF", "yellow" = "#FFFF00")) +
  my_theme() +  # Assuming this is a user-defined function for theming
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size=30, color = "#E4D96F", hjust = 0.5, vjust = 0.5, face = "bold", family = 'nosifer'))

# Added the title in Figma
