library(tidyverse)
library(scales)
library(MetBrewer)
library(janitor)
library(lubridate)
library(worldtilegrid)
library(showtext)

font_add(family = "Roboto", regular = "Fonts/roughflannel.otf")
showtext_auto()

pop <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

pop <- pop |> 
  rename(name = coo_name)

pop <- pop |> 
  mutate(name = case_when(name == "Dominican Rep."~"Dominican Republic",
                name == "Saint Kitts and Nevis"~"St. Kitts & Nevis",
                name == "Venezuela (Bolivarian Republic of)"~"Venezuela",
                name == "Bolivia (Plurinational State of)"~"Bolivia",
                name == "Saint Lucia"~"St. Lucia",
                name == "Saint Vincent and the Grenadines"~"St. Vincent & the Grenadines",
                name == "Trinidad and Tobago"~"Trinidad & Tobago",
                name == "United Kingdom of Great Britain and Northern Ireland"~"Great Britain and Northern Ireland",
                name == "Cote d'Ivoire"~"Côte d'Ivoire",
                name == "Netherlands (Kingdom of the)"~"Netherlands",
                name == "Czechia"~"Czech Republic",
                name == "Bosnia and Herzegovina"~"Bosnia & Herzegovina",
                name == "Dem. Rep. of the Congo"~"Congo (Democratic Republic of the)",
                name == "Serbia and Kosovo: S/RES/1244 (1999)"~"Serbia",
                name == "Central African Rep."~"Central African Republic",
                name == "Eswatini"~"Swaziland",
                name == "North Macedonia"~"Macedonia",
                name == "United Rep. of Tanzania"~"Tanzania",
                name == "Rep. of Moldova"~"Moldova (Republic of)",
                name == "Türkiye"~"Turkey",
                name == "Syrian Arab Rep."~"Syria",
                name == "Iran (Islamic Rep. of)"~"Iran (Islamic Republic of)",
                name == "Dem. People's Rep. of Korea"~"North Korea",
                name == "Rep. of Korea"~"South Korea",
                name == "Lao People's Dem. Rep."~"Lao People's Democratic Republic",TRUE ~ name))

df <- left_join(wtg, pop, by = "name")
head(df)

df_2022 <- df |> 
  filter(year == 2022)

p1 <- df_2022 |> 
  group_by(name) |> 
  summarise(total = sum(refugees)) |> 
  ggplot(aes(country = name,  fill = total)) +
  geom_wtg(border_col = "black", border_size = 1) +
  geom_text(aes(label = stat(alpha.2)), stat = "wtg", size = 10, family = "Roboto", face = "bold") + 
  scale_fill_gradient(low = "#ccd5da", high = "#4f637c",labels = scales::comma) +
  guides(fill = guide_colorsteps(labels = scales::comma), color = "black") + 
  theme_void() +
  theme(plot.title = element_text(color = "black", size = 200, face = "bold", family = "Roboto")) +
  theme(plot.subtitle = element_text(color = "black", size = 100, face = "bold", family = "Roboto")) +
  theme(plot.background  = element_rect(fill = "#eef1f4", color = "#eef1f4")) + 
  theme(axis.text.x = element_blank()) + 
  theme(axis.text.y = element_blank()) + 
  theme(legend.position = "top") +
  theme(legend.margin = margin(t = 30, unit = "pt")) + 
  theme(legend.background = element_rect(fill = "#eef1f4", color = "#eef1f4")) +
  theme(legend.text = element_text(size = 30, face = "bold", color = "#030303")) +
  theme(legend.justification = "left") +
  theme(legend.title = element_blank()) +
  theme(legend.key.width = unit(3.5, 'cm')) +
  labs(title = "Crossing Borders",
       subtitle = "A World Map of Total Refugee Numbers by Nation (2022)")
p1

# Added the image and the annotation using Figma
