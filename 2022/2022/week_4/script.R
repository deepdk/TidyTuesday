library(tidyverse)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

glimpse(ratings)
glimpse(details)


details %>% 
  select(boardgamecategory) %>%
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "[[:punct:]]"),
         boardgamecategory = str_trim(boardgamecategory)) %>% 
  filter(!is.na(boardgamecategory)) %>% 
  mutate(boardgamecategory = fct_lump(boardgamecategory, 10)) %>% 
  count(boardgamecategory, sort = TRUE, name = "counts") %>% 
  filter(boardgamecategory != "Other") %>%
  

  ggplot(aes (x = reorder(boardgamecategory, +counts), y = counts )) +
  geom_col(aes(fill = boardgamecategory), show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = c("white", "black")) +
  scale_fill_manual(values = c("#880303",
                               "#f0b603",
                               "#492d01",
                               "#ffccdd",
                               "#ff0000",
                               "#448aa4",
                               "#287653",
                               "#f5decd",
                               "#c53145",
                               "#2e2d35")) +
  theme(legend.position = "none",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank()
        ) +
  labs(
    title = "Top 10 Board Game Categories",
    subtitle = NULL,
    x = element_blank(),
    y = element_blank(),
    color = NULL,
    fill = NULL)





library(janitor)
boardgames <- ratings %>% left_join(details, by = 'id') %>% clean_names()


boardgames_category <- boardgames %>% 
  select(id, yearpublished, boardgamecategory, thumbnail, average) %>% 
  filter( yearpublished > 2011) %>% 
  separate_rows(boardgamecategory, sep = ",") %>% 
  mutate(boardgamecategory = str_remove_all(boardgamecategory, "[[:punct:]]"),
         boardgamecategory = str_trim(boardgamecategory)) %>% 
  filter(!is.na(boardgamecategory)) %>% 
  group_by(boardgamecategory) %>% 
  summarise(rating = mean(average, na.rm = TRUE),
            n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>%
  mutate(boardgamecategory = fct_reorder(boardgamecategory, rating)) %>% 
  head(20)

boardgames_category %>% 
  ggplot(aes(boardgamecategory, rating, color = '#473C8B')) +
  geom_point(color = '#8B3626', size = 2) +
  geom_segment(aes(x = boardgamecategory, xend = boardgamecategory, 
                   y = 0, yend = rating, color = '#473C8B'),
               size = 0.8) +
  geom_label(aes(boardgamecategory, rating , label = signif(rating)), 
             colour = "darkred", nudge_y = 0.35, size = 2)+
  coord_flip() +
  
  theme(plot.margin = margin(rep(17, 4)),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 20, family = "dance", hjust = 0.5, color = "#473C8B"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.5, family = "abel", color = "#473C8B"),
        plot.caption = element_text(color = "grey40", size = 12),
        axis.text.x = element_text(size = 11, family = "abel", color = "grey10"),
        axis.text.y = element_text(size = 11, family = "abel", color = "grey10"),
        axis.ticks = element_blank(),
        legend.position = "none") +
  labs(x = element_blank(),
       y = element_blank(),
       title = "Top rated boardgame categories",
       subtitle = "From year 2011- 2021")
