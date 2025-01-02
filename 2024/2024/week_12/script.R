library(tidyverse)
library(scales)
library(janitor)
library(MetBrewer)
library(showtext)
library(ggrepel)
library(stringr)
library(ggbump)
library(cropcircles)
library(ggimage)
library(svglite)
library(patchwork)
library(grid)

font_add_google("Chivo", "chivo")
showtext_auto()

mutant_moneyball <- read_csv("D:/TidyTuesday_2024/week_12/mutant.csv") |> 
  clean_names()

mutant_moneyball$member <- str_to_title(str_replace_all(mutant_moneyball$member, "(?<=[a-z])([A-Z])", " \\1"))

mutant_moneyball <- mutant_moneyball |> 
  mutate(image = crop_hex(image_url, border_colour = 0.5, bg_fill = "white"))

mutant_moneyball |>
  select(member, image, total_issues60s, total_issues70s, total_issues80s, total_issues90s) |> 
  pivot_longer(cols = c(3:6),
               names_to = "name",
               values_to = "value") 

my_theme <- function() {
  
  # Colors
  color.background = "white"
  color.text = "#030303"
  
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
    theme(plot.title       = element_text(color="#e11d1d", size=200, hjust = 0.5, family = 'chivo'))+
    theme(plot.subtitle    = element_text(color=color.text, size=60, hjust = 0.5, family = 'chivo'))+
    theme(plot.caption     = element_text(color=color.text, size=15,  hjust = 0.5, family = 'chivo'))+
    theme(axis.title.y = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x      = element_text(size=30, color = color.text, hjust = 0.5, vjust = 0.5, family = 'chivo')) +
    theme(axis.text.y      = element_text(size=30, color = color.text, family = 'chivo')) +
    theme(strip.text       = element_text(size=65, color = "#1C90BF", hjust = 0.5, vjust = 0.5, family = 'chivo')) 
}

mutant_moneyball |>
  select(comic_name, image_url, total_issues60s, total_issues70s, total_issues80s, total_issues90s) |> 
  rename("60s" = "total_issues60s",
         "70s" = "total_issues70s",
         "80s" = "total_issues80s",
         "90s" = "total_issues90s") |> 
  pivot_longer(cols = c(3:6),
               names_to = "name",
               values_to = "value") |> 
ggplot(aes(name, value)) +      
geom_bar(stat="identity",fill=grad_ungroup) +
geom_image(mapping=aes(y=75,x=1,image=image_url), size=0.25) +
#geom_text(aes(label = value), size = 10) +
#scale_fill_manual(values = met.brewer("Archambault",4))+
facet_wrap(~comic_name) +
my_theme() +
  labs(title = "X-Men Mutant MoneyBall",
       subtitle = "Total number of issues each X-Men member appeared in between 1963 and 1992.")

