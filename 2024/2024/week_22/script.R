library(tidyverse)
library(scales)
library(janitor)
library(lubridate)
library(showtext)
library(gt)
library(reactablefmtr)
library(webshot2)

f1 <- font_add_google("Raleway", "raleway")
showtext_auto()

harvest_2020 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2020.csv')
harvest_2021 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-05-28/harvest_2021.csv')

harvest <- bind_rows(harvest_2020, harvest_2021)

df <- harvest |> 
  mutate(vegetable = case_when(
    vegetable == "pumpkin" ~ "pumpkins",
    TRUE ~ vegetable
  )) |> 
  mutate(vegetable = case_when(vegetable == "apple" ~ "apples", TRUE ~ vegetable)) |> 
  mutate(vegetable = case_when(vegetable == "Swiss chard" ~ "swiss chard", TRUE ~ vegetable)) |> 
  mutate(vegetable = case_when(vegetable == "tomatillos" ~ "tomatoes",TRUE ~ vegetable)) |> 
  mutate(month = month(date)) |> 
  mutate(vegetable = str_to_title(vegetable))
df

emoji_lookup <- tibble::tribble(
  ~vegetable, ~emoji,
  "Swiss Chard", "🥬",
  "Apples", "🍎",
  "Asparagus", "🥦",
  "Basil", "🌿",
  "Beans", "🥜",
  "Beets", "🍠",
  "Broccoli","🥦",
  "Cabbage","🥬",
  "Carrots","🥕",
  "Chives","🌿",
  "Cilantro","🌿",
  "Corn","🌽",
  "Cucumbers","🥒",
  "Dill","🌿",
  "Edamame","🌱",
  "Garlic","🧄",
  "Hot Peppers","🌶️",
  "Jalapeño","🌶️",
  "Kale","🥬",
  "Kohlrabi","🥬",
  "Lettuce","🥬",
  "Mint","🥬",
  "Onions","🧅",
  "Oregano","🌿",
  "Peas","🫛",
  "Peppers","🌶",
  "Potatoes","🥔",
  "Pumpkins","🎃",
  "Radish","🌱",
  "Raspberries","🍒",
  "Rutabaga","🌱",
  "Spinach","🥬",
  "Squash","🎃",
  "Strawberries","🍓",
  "Sweet Potato","🍠",
  "Tomatoes","🍅",
  "Watermelon","🍉",
  "Zucchini","🌱"
  )

df_with_emojis <- df %>%
  left_join(emoji_lookup, by = "vegetable")
df_with_emojis

df1 <- df_with_emojis |> 
  mutate(weight = as.integer(weight)) |> 
  group_by(vegetable, emoji) |> 
  summarise(Harvest_weight = list(weight)) |> 
  rename("Vegetable" = "vegetable") |> 
  rename("Emoji" = "emoji")
df1

reactable(
  df1,
  columns = list(
    Vegetable = colDef(maxWidth = 120),
    Emoji = colDef(maxWidth = 90),
    Harvest_weight = colDef(name = "Harvest weight in grams" ,maxWidth = 300,
      cell = react_sparkline(
        df1,
        height = 40,
        line_width = 1.5,
        show_area = TRUE,
        tooltip_type = 1,
        line_color = "#8ac926",
        area_opacity = 0.3
      )
    )
  )
) |> 
  add_title(
    title = reactablefmtr::html("Lisa's Garden Harvest <img src='https://cdn-icons-png.freepik.com/512/5579/5579518.png' alt='Palmer Penguins' width='50' height='50'>"),
    font_size = 40
  ) %>% 
  add_subtitle(
    subtitle = "Total daily harvest from Lisa Lendway's vegetable garden, for the year 2020-2021.",
    font_size = 15,
    font_style = "normal",
    font_color = '#000000',
    margin = reactablefmtr::margin(t=10,r=0,b=15,l=0)
  ) |> 
  add_source("Table created by: Deepali Kank •  Data: {gardenR} by Lisa Lendway", font_size = 12) |> 
  google_font("Raleway", font_weight = 400) |> 
  save_reactable_test("lisa_garden.html","lisa_garden.html")
