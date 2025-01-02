library(tidyverse)
library(ggbump)
library(showtext)
library(cowplot)
library(rcartocolor)

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

trimmer <- function(x, break_limit) {     
  sapply(strwrap(x, break_limit, simplify=FALSE), paste, collapse="\n")
  
plot_data <- breed_rank_all %>%
  filter(`2020 Rank` %in% 1:10) %>% 
  pivot_longer(cols = 2:9) %>%
  mutate(year = as.numeric(str_sub(name, 1, 4)))
  
my_theme <- function() {

  # Colors
  color.background = "gray90"
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
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

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

ggplot(data = plot_data, aes(x = year, y = value, group = Breed)) +
  geom_point(aes(color = Breed, alpha = 1), size = 3) +
  geom_bump(aes( color = Breed))+
  scale_y_reverse(breaks = 1:nrow(plot_data)) +
  geom_text(data = filter(plot_data, year == 2020), 
            mapping = aes(label = str_wrap(Breed, 60), color = Breed), 
            hjust = -0.2) +
  scale_y_reverse(breaks = c(1:13)) +
  scale_x_continuous(breaks = c(2013:2020), limits = c(2013, 2026))+
  scale_colour_carto_d(palette = "Vivid")+
  theme(legend.position = "none") +
  labs(x = "Year",
       y = "Rank",
       title = "Most Popular Dog Breeds",
       subtitle = "Breeds ranked by year") +
  my_theme()
