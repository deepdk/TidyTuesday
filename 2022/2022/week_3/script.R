library(tidyverse)
library(lubridate)
library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(devtools)
library(factoextra)
library(viridis)
library(ggrepel)
library(ggpubr)
library(jpeg)
library(pacman)
library(grid)


df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

glimpse(df)

df %>%
  count(country_of_bean_origin, sort = TRUE)

df %>%
  count(company_manufacturer, sort = TRUE)

df %>%
  count(specific_bean_origin_or_bar_name, sort = TRUE)

df %>%
  count(most_memorable_characteristics, sort = TRUE)


image <- readJPEG("G:/istockphoto-463813283-170667a.jpg")

df %>% 
  group_by(country_of_bean_origin) %>% # Group by origin
  filter(n() > 10) %>% # Limit to those with at least 10 observations
  mutate(count = n()) %>% # Add the count column
  ggplot(aes(x = reorder(country_of_bean_origin, count), y=count)) + 
  ggtitle("Countries Producing Chocolate Beans") +
  labs(x = "Country",y = "Count")+
  scale_fill_continuous(guide = FALSE) +
  annotation_custom(rasterGrob(image, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc")), 
                    -Inf, Inf, -Inf, Inf) +
  geom_bar(stat="identity", fill = "#BD9A7A", position = "dodge", width = .75, colour = 'white') +
  geom_text(aes(label=paste0(round(count))), hjust =0.5,vjust = -0.1, color="white", size=4,fontface = 2) +
  theme(axis.text.x = element_text(angle=45, hjust = 1)) + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5))
