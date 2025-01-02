library(tidyverse)


erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')


glimpse(erasmus)

erasmus %>%
  count(receiving_organization, sort = TRUE)

erasmus %>%
  count(receiving_city, sort = TRUE)

erasmus %>%
  count(sending_city, sort = TRUE)

erasmus %>%
  count(participant_gender, sort = TRUE)

erasmus %>%
  mutate(
    receiving_country_code = case_when(receiving_country_code == 'EL' ~ 'GR',
                                       receiving_country_code == 'UK' ~ 'GB',
                                       TRUE ~ receiving_country_code)
  )

countries_codes <- readr::read_csv("https://gist.githubusercontent.com/tadast/8827699/raw/f5cac3d42d16b78348610fc4ec301e9234f82821/countries_codes_and_coordinates.csv")

women_receiving_countries <- erasmus %>%
  filter(participant_gender == "Female") %>%
  group_by(receiving_country_code) %>%
  summarise(
    nb_participants = n()
  ) %>%
  ungroup() %>%
  left_join(countries_codes, by = c('receiving_country_code' = 'Alpha-2 code')) %>%
  mutate(
    Country = ifelse(str_detect(Country, "Macedonia"), "Macedonia", Country)
  )

women_sending_countries <- erasmus %>%
  filter(participant_gender == "Female") %>%
  group_by(sending_country_code) %>%
  summarise(
    nb_participants = n()
  )%>%
  ungroup() %>%
  left_join(countries_codes, by = c('sending_country_code' = 'Alpha-2 code')) %>%
  mutate(
    Country = ifelse(str_detect(Country, "Macedonia"), "Macedonia", Country)
  )

my_theme <- function() {
  
  # Colors
  color.background = "#030303"
  color.text = "#FFFFFF"
  
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
    theme(legend.background = element_rect(fill="#FFFACD", 
                                           size=0.5, linetype="solid"))+
    
    
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=10, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



complete.cases(women_receiving_countries)
df <- na.omit(women_receiving_countries)
df1 <- na.omit(women_sending_countries)


main_plot <- df %>% 
    ggplot(aes(reorder(Country, nb_participants),nb_participants)) + 
    geom_col(fill = "#35978F")+
    coord_flip()+
    my_theme()+
  labs(x = "Country",
       y = "Count",
       title = "Countries Accepting women for Erasmus Program")
main_plot

main_plot1 <- df1 %>% 
  ggplot(aes(reorder(Country, nb_participants),nb_participants)) + 
  geom_col(fill = "#DFC27D")+
  coord_flip()+
  my_theme()+
  labs(x = "Country",
       y = "Count",
       title = "Countries Sending women for Erasmus Program")
main_plot1

main_plot + main_plot1
