library(tidyverse)
library(lubridate)
library(showtext)
library(patchwork)

bioc <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/bioc.csv')
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

glimpse(bioc)

glimpse(cran)

cran %>%
  count(package, sort = TRUE)
 
bioc %>%
  count(package, sort=TRUE)

bioc %>%
  count(rmd)

bioc %>%
  group_by(package) %>%
  summarise(rmd = n()) %>%
  arrange(desc(rmd))

vignette_counts <-
  bioc %>%
  group_by(package) %>%
  summarise(
    release_date = first(date),
    releases = n(),
    vignettes = last(rnw) + last(rmd)
  ) %>%
  arrange(desc(vignettes))%>%
  head(10)


new_pkg <- bioc %>%
  mutate(year = year(date)) %>%
  group_by(package) %>%
  mutate(first_release = min(year)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  count(first_release) %>%
  filter(first_release > 1970)

new_pkg1 <- bioc %>%
  group_by(package) %>%
  summarise(rmd = n()) %>%
  arrange(desc(rmd)) %>%
  head(20)
 new_pkg1
 
 
 my_theme <- function() {
   
   # Colors
   color.background = "#030303"
   color.text = "#FFBBFF"
   
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
     theme(legend.position = "None") +
     theme(legend.background = element_blank())+
     
     
     
     # Format title and axis labels
     theme(plot.title       = element_text(color=color.text, size=10, face = "bold")) +
     theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
     theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
     theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
     theme(axis.text.y      = element_text(size=10, color = color.text)) +
     theme(strip.text       = element_text(face = "bold")) +
     
     # Plot margins
     theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
 }
 
 
 p1 <- ggplot(data = new_pkg) +
   geom_point(aes(x = first_release, y = n),
              colour = "#FFC0CB", size = 4) +
   geom_line(aes(x = first_release, y = n),
             colour = "#FFC0CB", size = 0.75) +
   scale_x_continuous(breaks = seq(2001, 2021, 5))+
   labs(x = "Year",
        y = "Count",
        title = "New Packages Released each year")+
   my_theme()
 p1
  


p2 <- ggplot(data = new_pkg1) +
  geom_col(aes(x = reorder(package, rmd), y = rmd),
             fill = "#FFC0CB") +
  labs(x = "Package",
       y = "Count",
       title = "Packages having maximum releases")+
  coord_flip()+
  my_theme()
p2



p4 <- ggplot(data = vignette_counts) +
  geom_point(aes(x = reorder(package, vignettes), y = vignettes, color = '#FFC0CB')) +
  geom_segment(aes(x = package, xend = package, 
                   y = 0, yend = vignettes, color = '#FFC0CB'),
               size = 0.8) +
  geom_label(aes(package, vignettes , label = signif(vignettes)), 
             colour = "darkred", nudge_y = 0.35, size = 2)+
  labs(x = "Package",
       y = "Count",
       title = "Packages having maximum number of Vignettes")+
  my_theme()+
  coord_flip()
p4

p34 <- p2 | (p1 / p4) + plot_annotation(title = "Bioconductor Packages")
p34


p34 +plot_annotation(theme = theme_gray(base_family = "mono"))
