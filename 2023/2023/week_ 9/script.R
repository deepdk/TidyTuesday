library(tidyverse)
library(MetBrewer)
library(scales)

afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')

lan1 <- left_join(language_countries,languages, on = 'language_iso_code')
head(lan1)

final <- left_join(afrisenti, lan1, on = 'language_iso_code')

color.text = "#363636"

final %>% 
  group_by(country, language) %>% 
  count(label) %>% 
  ggplot(aes(language, n, fill = label))+
  geom_col(position = "fill", color = "black")  +
  scale_fill_manual(values = met.brewer("Degas",3))+
  facet_wrap(~country)+
  coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  theme_light()+
  theme(legend.position = "top") +
  theme(legend.background = element_rect(fill="white", color="white"))+
  theme(legend.text = element_text(size = 20, face = "bold", color=color.text))+
  theme(legend.justification = "center")+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(1, 'cm'))+
  theme(panel.background = element_rect(fill="white", color="white")) +
  theme(plot.background  = element_rect(fill="white", color="white")) +
  theme(panel.border     = element_rect(color="white")) +
  theme(strip.background = element_rect(fill="white", color="white"))+
  theme(strip.background = element_rect(color="black", fill="#EEC591", size=1, linetype="solid"))+
  theme(strip.text.x = element_text(size = 30, color = "black", family = "Roboto"))+
  theme(plot.title       = element_text(color=color.text, size=150, face = "bold", family = 'Roboto'))+
  theme(plot.subtitle    = element_text(color=color.text, size=30, face = "bold", family = 'Roboto'))+
  theme(plot.caption     = element_text(color=color.text, size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
  theme(axis.title.x     = element_blank()) +
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.x      = element_text(size=20, color = color.text, hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
  theme(axis.text.y      = element_text(size=20, color = color.text, face = "bold", family = 'Roboto'))+
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))+
  labs(title = "Afrisenti",
       subtitle = "Sentiment analysis by country for under-represented languages covering 110,000+ annotated \n tweets in 14 African languages",
      caption = "Data Source : African Language Sentiment/TidyTuesday week 9 2023 \n Graphic : Deepali Kank")
