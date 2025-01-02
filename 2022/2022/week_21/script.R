library(tidyverse)
library(reactablefmtr)

fifteens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/fifteens.csv')

df <- fifteens %>%
  separate(date, c('year','month','day'))
head(df)

df1 <- df %>%
  filter(tournament == 'World Cup')

table <- df1 %>%
  group_by(year)%>%
  filter(test_no == max(test_no), year != 2000, year!= 2005, year != 2009, year!=2013, year!=2016)%>%
  select(year, team_1, team_2, score_1, score_2,margin_of_victory,winner)%>%
  ungroup()
table

data <- table %>%
  mutate(
    img = case_when(
      winner == "United States" ~
        "https://i0.wp.com/palmerluckey.com/wp-content/uploads/2020/09/american-flag-logo-png-4.png",
      winner == "England" ~
        "https://wiki2.railml.org/images/b/b8/UK_flag.png",
      winner == "New Zealand" ~
        "https://www.freepngimg.com/thumb/new_zealand/5-2-new-zealand-flag-png-file-thumb.png",
      TRUE ~ "NA"))

data <- rename(data, Year = year,
               Team_1 = team_1,
               Team_2 = team_2,
               Score_1 = score_1,
               Score_2 = score_2,
               Margin = margin_of_victory,
               Winner = winner,
               Flag = img)

rugby_table <- reactable(data,
          theme = sanfran(),
          columns = list(
            Flag = colDef(cell = embed_img()),
            Score_1 = colDef(style = color_scales(data)),
            Score_2 = colDef(style = color_scales(data)),
            Margin = colDef(style = color_scales(data)))) %>%
  google_font(font_family = "Poppins",
              font_weight = 700)%>%
  add_title(
    title = reactablefmtr::html("Women's Rugby Fifteens <img src='https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Ftse4.mm.bing.net%2Fth%3Fid%3DOIP.x9jHypkzF49t_ej0gy1mpwHaHa%26pid%3DApi&f=1' alt='Palmer Penguins' width='100' height='50'>"),
    align = "center",
    font_color = "#8B1A1A",
    font_size = 35,
    font_style = "normal",
    font_weight = "bold",
    text_decoration = NULL,
    text_transform = NULL,
    letter_spacing = NULL,
    word_spacing = NULL,
    text_shadow = NULL,
    background_color = "#FFF0F5",
    margin = NULL) %>% 
  
  add_subtitle(
    subtitle = 'World Cup Winners',
    font_size = 25,
    font_color = '#5D478B',
    background_color = "#FFF0F5",
    align = "center",
    margin = reactablefmtr::margin(t=10,r=0,b=15,l=0)
  )%>%
  add_source("Data Source : TidyTuesday/Week_21_2022/Women's Rugby", align = 'center',  margin = reactablefmtr::margin(t=10,r=0,b=15,l=0))

rugby_table

save_reactable_test(rugby_table, "rugby_table.png")
