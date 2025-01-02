install.packages('rtrek')
install.packages('trekfont')

library(tidyverse)
library(ggimage)
library(png)
library(jpeg)
library(cropcircles)
library(rtrek)
library(showtext)
library(trekfont)

annotate <- ggplot2::annotate

font_add("StarDown", system.file(paste0("fonts/StarDown.ttf"), package = "trekfont"))
showtext_auto()

library(repr)
options(repr.plot.width = 20, repr.plot.height =17)

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
    theme(plot.title       = element_text(color=color.text, size=40, face = "bold", hjust = 0.5, family = 'StarDown'))+
    theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", hjust = 0.5, family = 'StarDown'))+
    theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'StarDown'))+
    theme(axis.title.x     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'StarDown')) +
    theme(axis.title.y     = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'StarDown')) +
    theme(axis.text.x      = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'StarDown')) +
    theme(axis.text.y      = element_text(size=25, color = color.text, face = "bold", family = 'Roboto')) +
    theme(strip.text       = element_text(size=25, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'StarDown')) +

# Plot margins
theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

scriptData <- st_transcripts()
pat <- "('s\\s|\\s\\(|\\sV\\.).*"
x <- filter(scriptData, format == "episode" & series == "TNG") %>% 
  unnest(text) %>%
  select(season, title, character, line) %>%
  mutate(character = gsub(pat, "", character)) %>%
  group_by(season, title, character) %>%
  summarize(lines = n(), words = length(unlist(strsplit(line, " "))))

totals <- group_by(x, character) %>% 
  summarize(lines = sum(lines), words = sum(words)) %>% 
  arrange(desc(words)) %>% top_n(8)
totals <- totals %>%
mutate(image = case_when(character == "Picard"~"/kaggle/input/star-trek/Picard.jpg",
                         character =="Data"~"/kaggle/input/star-trek/Data.png",
                         character == "Riker"~"/kaggle/input/star-trek/Riker.png",
                         character == "Geordi"~"/kaggle/input/star-trek/Geordi.jpg",
                         character == "Beverly"~"/kaggle/input/star-trek/Beverly.jpg",
                         character == "Troi"~"/kaggle/input/star-trek/Troi.png",
                         character == "Worf"~"/kaggle/input/star-trek/Worf.jpg",
                         character == "Wesley"~"/kaggle/input/star-trek/Wesley.jpg"))
 
totals <- totals %>%
mutate(images_cropped = circle_crop(image))

id <- totals$character
chr <- factor(totals$character, levels = id)
uniform_colors <- c("#5B1414", "#AD722C", "#1A6384")
ulev <- c("Command", "Operations", "Science")
uniform <- factor(ulev[c(1, 2, 1, 2, 3, 3, 2, 3)], levels = ulev)
totals <- mutate(totals, character = chr, uniform = uniform)

p1 <- ggplot(totals, aes(reorder(character, words),words, fill = uniform)) + 
  geom_bar(stat = "identity",width = 0.02) + 
  geom_image(mapping=aes(y=words, x=character, image=images_cropped, color = character), size=0.059, asp=1.5)+
  geom_image(mapping=aes(y=words, x=character, image=images_cropped), size=0.05, asp=1.5)+
  geom_text(aes(label = character), size = 8,hjust = 1.5, vjust = -1.5,color = "#1874CD", fontface = "bold", family = "StarDown")+
  scale_fill_manual(values = uniform_colors)  +
  scale_color_manual(values = c("#5B1414", "#AD722C", "#5B1414","#AD722C", "#1A6384", "#1A6384","#AD722C", "#1A6384"))+
  coord_flip()+
  annotate("text", x = 2.5, y = 120000, label = "Total Estimaed Words Spoken By Each \nCharacter",size = 10, fontface = "bold",family = "StarDown", color = "#1874CD")+
  my_theme()+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size = 20, color = "#1874CD"))+
  theme(axis.title.x = element_text(size = 25, color = "#1874CD"))+
  theme(legend.position = "none")
  

url <- "https://wallpapercave.com/wp/Y0StpXn.jpg"
ggbackground(p1, url)

#Added the title logo using figma
