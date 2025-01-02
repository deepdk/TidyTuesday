library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(viridis)
library(patchwork)
library(showtext)

sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

glimpse(sports)

sports %>%
  count(sports, sort = TRUE)

sports %>%
  count(year, sort = TRUE)

sport_sum <- sports %>%
  group_by(sports) %>%
  summarise(
    revwomen = mean(rev_women, na.rm = TRUE))

view(sport_sum)

sport_sum1 <- sports %>%
  group_by(sports)%>%
  summarise(revmen = mean(rev_men, na.rm = TRUE))

view(sport_sum1)

tmp <- sport_sum %>%
  filter(!is.na(revwomen)) %>%
  arrange(desc(revwomen)) %>%
  mutate(sports=factor(sports, sports))
view(tmp)


tmp1 <- sport_sum1 %>%
  filter(!is.na(revmen)) %>%
  arrange(desc(revmen)) %>%
  mutate(sports=factor(sports, sports))
view(tmp1)

tmp$id=seq(1, nrow(tmp))
label_data <- tmp


number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar    
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)



my_theme <- function() {
  
  # Colors
  color.background = "#FFFFFF"
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
    theme(panel.grid.major.x = element_blank())+
    theme(panel.grid.minor.x = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "bottom") +
    theme(legend.background = element_rect(fill="#FFFFFF", 
                                           size=0.5, linetype="solid"))+
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_blank()) +
    theme(axis.title.y     = element_blank()) +
    theme(axis.text.x      = element_blank()) +
    theme(axis.text.y      = element_blank()) +
    theme(strip.text       = element_blank()) +
    
    # Plot margins
    theme(plot.margin = unit(rep(-1,7), "cm"))
}


# Start the plot
p <- ggplot(tmp, aes(x=as.factor(id), y=revwomen)) +      
  geom_bar(stat="identity", fill=alpha("#FF6A6A", 0.9)) +
  ylim(-400000,600000) +
  labs(title = "Women")+
  theme_minimal()+
  theme(plot.title = element_text(size = 10, hjust = 0.5,vjust = -115, face = "bold"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=id, y=revwomen+10, label=sports, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE )

p


label_data1 <- tmp1

tmp1$id=seq(1, nrow(tmp1))
number_of_bar <- nrow(label_data1)
angle <-  90 - 360 * (label_data1$id-0.5) /number_of_bar    
label_data1$hjust<-ifelse( angle < -90, 1, 0)
label_data1$angle<-ifelse(angle < -90, angle+180, angle)

p1 <- ggplot(tmp1, aes(x=as.factor(id), y=revmen)) +       
  geom_bar(stat="identity", fill=alpha("#5D478B", 0.9)) +
  ylim(-4000000,6500000) +
  labs(title = "Men")+
  theme_minimal()+
  theme(plot.title = element_text(size = 10, hjust = 0.5,vjust = -115, face = "bold"),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  coord_polar(start = 0) +
  geom_text(data=label_data1, aes(x=id, y=revmen+10, label=sports, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data1$angle, inherit.aes = FALSE ) 
p1


p+p1+
  plot_annotation(
    title = 'Which sport earns more revenue?',
    subtitle = "IceHockey earns maximum revenue for women and Football for men",
    caption = "Data Source: Collegiate Sports In USA - #TidyTuesday Week 13",
    theme = theme(plot.title = element_text(size = 25, hjust = 0.5),
                  plot.subtitle = element_text(size = 15, hjust = 0.5),
                  plot.caption = element_text(size = 10, hjust = 0.5),
                  plot.background = element_rect(fill = "#EDEDED",
                                   color = "#EDEDED", size = 2)))
  

