# Importing Libraries
library(tidyverse)
library(patchwork)
library(ggtext)
library(ggthemes)
library(scales)
library(ggimage)
library(png)
library(cowplot)

# Adding font
font_add_google('Fira Sans', 'firasans')
showtext_auto()

# Reading datafile
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')
head(df)

# Plotting first plot
df1 <- df %>%
group_by(HabType, Detection)%>%
count(HabType)

p1 <- ggplot(data =df1, aes(HabType, n, fill = Detection))+
geom_bar(stat = "identity", position = "fill") +
scale_y_continuous(labels = scales::percent_format())+
scale_fill_manual(values = c("#00FFFF","#FFB90F","#BF3EFF"))+
theme_light()+
theme(axis.text.x  = element_text(size=25, color = "#030303", hjust = 0.5, vjust = 0.5,face = "bold", family = 'firasans')) +
theme(axis.text.y = element_text(size=25, color = "#030303", face = "bold", family = 'firasans'))+
theme(axis.title.x = element_blank())+
theme(axis.title.y = element_blank())+
theme(legend.position = "top")+
theme(legend.background = element_rect(fill="#FFFFFF", color="#FFFFFF"))+
theme(legend.text = element_text(size = 30, color="#030303", family = "firasans"))+
theme(legend.justification = "center")+
theme(legend.title = element_blank())+
theme(legend.key.size = unit(1, 'cm'))+
theme(plot.title   = element_text(color="#030303", size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
labs(title = "Detection")
p1

# Plotting second file
my_image <- readPNG("../input/oregonfrog/frog.png", native = TRUE)

data <- data.frame(
  category=c("Pond", "Reservoir", "River"),
  count=c(95, 173, 43)
)
 
data$fraction <- data$count / sum(data$count)
data$ymax <- cumsum(data$fraction)
data$ymin <- c(0, head(data$ymax, n=-1))
data$labelPosition <- (data$ymax + data$ymin) / 2
data$label <- paste0(data$category)

p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_text( x=3.5, aes(y=labelPosition, label=label), size=8, color = "black") +
  scale_fill_manual(values=c("#00FFFF","#FFB90F","#BF3EFF")) +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_void() +
  theme(legend.position = "none")+
  theme(plot.title   = element_text(color="#030303", size=40, face = "bold", hjust = 0.5, family = 'firasans'))+
  labs(title = "Habitat")

p2 <- ggdraw() +
  draw_image(
    my_image, scale = 0.65
  ) +
  draw_plot(p)
 
 
# Adding text box 
title_text    <- 'Oregon Spotted Frogs (Rana Pretiosa)'

subtitle_text <- "Tracking of individual frogs location data and habitat use between September and late November of 2018."

caption_text  <- paste0("**#TidyTuesday:** 2022 Week 29 • **Source:** data.nber.org<br>",
                        "**Visualization:** Steven Ponce (@sponce1) • **Tools:** #rstats, #ggplot")

body_text_1 <- "The Oregon spotted frog (Rana pretiosa, meaning precious frog is a member of the frog family Ranidae of order Anura. It is a medium-sized aquatic frog endemic to the Pacific Northwest and historically well distributed in the Puget Trough/Willamette Valley province and the Cascade Mountains of south-central Washington and Oregon.It is relatively rare within its range and is listed globally as vulnerable. The Oregon spotted frog is a highly aquatic frog that seldom strays from areas of standing water. Bodies of water (i.e., wetlands, lakes and slow-moving streams) that included zones of shallow water with abundant emergent or floating aquatic plants are suitable for the Oregon spotted frogs. Mats of aquatic vegetation are used for basking on and escaping danger by diving beneath the cover of the vegetation. These habitats often provide a thin layer of unusually warm water which the frogs appear to prefer."

title_col      <- 'black'
subtitle_col   <- "black"
caption_col    <- "black"

P2 <- ggplot() +
    theme_void() +
    
    # title
    geom_textbox(aes(x = 0, y = 0),  
                 family      = "aleg",
                 label       = title_text,
                 color       = title_col,
                 size        = 30, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(9, "in"),
                 vjust       = -3,
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
                 
                 # subtitle
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'firasans',
                 label       = subtitle_text,
                 color       = title_col,
                 size        = 15, 
                 fill        = NA,     
                 box.color   = NA,
                 width       = unit(7, "in"),
                 vjust       = -1.5, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0.5,
                 fontface    ="bold", 
                 lineheight  = 1) +
                 
                 # text 1
    geom_textbox(aes(x = 0, y = 0),  
                 family      = 'firasans',
                 label       = body_text_1,
                 color       = title_col,
                 size        = 10, 
                 fill        = NA, 
                 box.color   = NA,
                 width       = unit(8, "in"),
                 vjust       = 0.7, 
                 valign      = 0.5,
                 hjust       = 0.5, 
                 halign      = 0,   
                 fontface    = 'plain',
    )
    
    P2 + (p2/p1)
