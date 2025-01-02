library(tidyverse)
library(paletteer)
library(showtext)
library(ggimage)
library(MerBrewer)

font_add(family = "Roboto", regular = "D:/HP laptop/Fonts/RobotoCondensed-Bold.ttf")
showtext_auto()

my_theme <- function() {
  
  # Colors
  color.background = "#E3D2C0"
  color.text ="#060303"
  
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
    theme(plot.title       = element_text(color=color.text, size=80, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.subtitle    = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
    theme(axis.title.x     = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.title.y     = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
# Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

name <- c("Mukesh Ambani","Steve Ballmer","Larry Ellison","Sergey Brin","Larry Page",
         "Warren Buffet","Bill Gates","Bernard Arnault and Family","Jeff Bezos","Elon Musk")
networth <- c(90.7,91.4,106.0,107.0,111.0,118.0,129.0,158.0,171.0,219.0)
source <- c("diversified","Microsoft","software","Google","Google","Berkshire Hathway","Microsoft","LVMH","Amazon","Tesla,SpcaeX")
images <- c("D:/HP laptop/forbes billionare/mukesh ambani fig.png","D:/HP laptop/forbes billionare/Steve-Ballmer-fig.png","D:/HP laptop/forbes billionare/larry ellison fig.png","D:/HP laptop/forbes billionare/sergey-brin-fig.png","D:/HP laptop/forbes billionare/Larry page fig.png",
           "D:/HP laptop/forbes billionare/warren buffet fig.png","D:/HP laptop/forbes billionare/bill gates fig.png","D:/HP laptop/forbes billionare/bernard-arnault fig.png","D:/HP laptop/forbes billionare/jeff bezos fig.png","D:/HP laptop/forbes billionare/elon musk figma.png")

dataframe <- data.frame(name, networth,source, images)
dataframe

dataframe %>%
ggplot(aes(reorder(name,networth),networth,fill = name))+
geom_bar(stat = 'identity', width = 0.02)+ 
geom_image(mapping=aes(y=networth, x=name, image=images,  color=name), size=0.049, asp=1.4)+
geom_image(mapping=aes(y=networth, x=name, image=images), size=0.04, asp=1.4)+
geom_text(aes(label = source), size = 3.5,hjust = 2.5, vjust = -1,color = '#573E3D', fontface = "bold", family = "Roboto")+
geom_text(aes(label = networth), size = 3.5,hjust = 2, vjust = -1,color = '#573E3D', fontface = "bold", family = "Roboto")+  
scale_y_continuous(labels = scales::dollar)+
scale_fill_manual(values = met.brewer("Degas",10))+
scale_color_manual(values = met.brewer("Degas",10))+
coord_flip()+
theme_minimal()+
theme(axis.title.y = element_blank())+
theme(legend.position = "none")+  
labs(x = "networth in billions",
     title = "Forbes World's Billionaires List",
    subtitle = "The Richest in 2022 (Networth in Billions)",
    caption = "Data Source : BOYD/TidyTuesday/week 1 2023     Graphic : Deepali Kank")
