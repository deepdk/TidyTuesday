library(tidyverse)
library(taylor)


# Prepare the data according to this R graph gallery circular bar plot chart. https://r-graph-gallery.com/circular-barplot.html
data <- taylor_album_songs |> 
  filter(!is.na(duration_ms)) |> 
  filter(!is.na(track_name)) |> 
  filter(!is.na(album_name)) |> 
  mutate(duration_ms = duration_ms/1000) |> 
  rename('individual' = 'track_name') |> 
  rename('group' = 'album_name') |> 
  rename('value' = 'duration_ms') |> 
  select(individual, group, value)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 20
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))
 
# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)
 
# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
 
# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
 
# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 500, xend = start, yend = 500), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 300, xend = start, yend = 300), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "white", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  #annotate("text", x = rep(max(data$id),6), y = c(100, 200, 300, 400,500,600), label = c("100", "200", "300", "400","500","600") , color="black", size=5 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  ylim(-500,700) +
  scale_fill_albums() +
  #scale_fill_manual(values = c("#D00000","#FFBA08","#CBFF8C","#8FE388","#1B998B","#3185FC","#5D2E8C","#46237A","#FF7B9C","#FF9B85")) + 
  theme_minimal() +
  theme(
    legend.position = "right",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm"),
    plot.background = element_rect(color = "white",fill = "white")
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 
P

# Added the title, subtitle, color legend and an image using Figma
 
