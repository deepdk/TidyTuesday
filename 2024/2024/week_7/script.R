library(tidyverse)
library(janitor)
library(ggsvg)
library(svglite)

my_theme <- function() {
  
  # Colors
  color.background = "#F2F2F2"
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
    theme(legend.title = element_text(
                                    color = "#030303",
                                    size = 15, face = "bold"))+
# Format title and axis labels
    theme(plot.title = element_text(color=color.text, size=40, face = "bold", hjust = 0.5))+
    theme(plot.subtitle = element_text(color=color.text, size=30, face = "bold", hjust = 0.5))+
    theme(plot.caption = element_text(color=color.text, size=20, face = "bold", hjust = 0.5))+
    theme(axis.title.x = element_text(size=12, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold")) +
    theme(axis.title.y     = element_text(size=12, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold")) +
    theme(axis.text.x      = element_text(size=12, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold")) +
    theme(axis.text.y      = element_text(size=12, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=12, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold")) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

gifts_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-13/gifts_age.csv')

p <- gifts_gender |> 
  pivot_longer(cols = c(3:9),
               names_to = "name",
               values_to = "value") |> 
  ggplot(aes(reorder(name, value), value)) +
  geom_line(linewidth = 4, alpha = 0.3) +
  #geom_point(aes(color = gender), size = 10) + # Uncommented this line
  geom_point_svg(aes(fill = gender), svg = svg_txt, size = 10) + # Assuming it works
  scale_fill_manual(values = c("#5e548e", "#fb6f92")) +
  scale_color_manual(values = c("#5e548e", "#fb6f92")) + # Added color scale for consistency
  coord_flip() +
  geom_text(aes(label = value), size = 5) +
  scale_y_continuous(breaks = seq(0,60, by = 10),limits = c(0,60))+
  my_theme() +
  theme(legend.position = "none") 
p

# Saved the chart as svg and edited in Figma
width_px = 500
height_px = 500
dpi = 96 
width_in_inches = width_px / dpi
height_in_inches = height_px / dpi
svglite("p.svg", width = width_in_inches, height = height_px / dpi)
print(p) # Plot output is captured and written to the file
dev.off()
