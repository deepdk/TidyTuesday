library(tidyverse)
library(gt)
library(webshot)

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

glimpse(news_orgs)
view(news_orgs)
 
country <- news_orgs%>%
  count(country, sort = TRUE)

state <- news_orgs%>%
  count(state, sort = TRUE)

news_orgs %>%
  count(state, sort = TRUE)
  
df <- news_orgs %>%
  filter(country == "Canada")

df1 <- df %>%
  count(city, sort = TRUE)

df2 <- df %>%
  filter(city == "Vancouver")

df2 %>%
  select(publication_name, tax_status_current, year_founded, total_employees, summary, is_owner_founder) %>%
  mutate(
    year_founded = as.numeric(year_founded),
    years_in_operation = 2022 - year_founded
  )%>%
  group_by(tax_status_current,) %>%
  arrange(
     publication_name,
    desc(years_in_operation), desc(year_founded), desc(total_employees),
    .by_groups = TRUE ) %>%
  gt() %>%
  tab_header(
    title = md("**Vancouver Publications**"),
    subtitle = md(" _Using data from [Projest Oasis](https://www.projectnewsoasis.com/publications) by way of Data is Plural")
  ) %>%
  cols_label(
    publication_name = md("**Publications**"),
    year_founded = md("**Founded**"),
    years_in_operation = md("**Years of Operations**"),
    total_employees = md("**Employees**"),
    summary = md("**Summary**"),
    is_owner_founder = md("**Owner is Founder or Not**")
  )%>%
  cols_move_to_end(summary)%>%
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_title(groups = "title")
    )%>%
  tab_options(table.font.size = px(15)) %>%
  opt_table_font(
    font = list(
      google_font(name = "Merriweather"),
      "Cochin", "Serif"
    )
  )%>%
  tab_style(
    style = cell_fill(color = "#F0FFFF"),
    locations = cells_body(columns = everything(), rows = everything())
  )%>%
  tab_style(
    style = cell_fill(color = "#FFE1FF"),
    locations = cells_row_groups(groups = everything())
  )%>%
  gtsave(
    "tab_2.png", expand = 5, zoom =2,
    path = "G:/tidytuesday/Week 14_Digital publications"
  )  
