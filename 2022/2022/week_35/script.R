library(tidyverse)
library(ggthemes)
library(usmap)
library(geomtextpath)
library(ggborderline)
library(MetBrewer)

font_add(family = "Roboto", regular ="../input/roboto-bold/RobotoCondensed-Bold.ttf" )
showtext_auto()

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv')

pell_df <- pell %>%
rename(abbr = state)
head(pell_df)

plot_data <- left_join(pell_df,
                       select(statepop, c(fips, abbr, full)),
                       by = "abbr")
plot_data$region <- tolower(plot_data$full)
head(plot_data)

plot_data <- left_join(pell_df,
                       select(statepop, c(fips, abbr, full)),
                       by = "abbr")
plot_data$region <- tolower(plot_data$full)

d1 <- plot_data %>%
group_by(year, full)%>%
summarise(total = sum(recipient),award_sum = sum(award))

region <- c("California","New York","Texas","Pennsylvania","Florida","Washington")

d1 %>%
filter(full %in% region)%>%
mutate(full = factor(full, levels = region, ordered = TRUE))%>%
ggplot(aes(x = year, y = total, color = full))+
geom_line(data = d1, aes(group = full, color = NULL), size = 0.1, color = "#CFCFCF")+
geom_borderline(size = 1, bordersize = 0.1)+
geom_labelline(aes(label = full, hjust = full, fill = full),
              linecolor = NA, size = 7, textcolor = 'white', family = "Roboto")+
scale_color_manual(values = met.brewer("Cross",6))+
scale_fill_manual(values = met.brewer("Cross",6))+
scale_hjust_discrete(range = c(0.05,0.055))+
theme_light()+
theme(panel.background = element_rect(fill="#292929", color="#292929")) +
theme(plot.background  = element_rect(fill="#292929", color="#292929")) +
theme(panel.border     = element_rect(color="#292929")) +
theme(panel.grid.major.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.minor.y = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(panel.grid.major.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL))+
theme(panel.grid.minor.x = element_line(colour = '#363636', size = 1, linetype = NULL, lineend = NULL)) +
theme(strip.background = element_rect(fill="#292929", color="#292929"))+
theme(plot.title       = element_text(color="#FFFFFF", size=100, face = "bold", family = 'Roboto'))+
theme(plot.subtitle    = element_text(color="#FFFFFF", size=50, face = "bold", family = 'Roboto'))+
theme(plot.caption     = element_text(color="#FFFFFF", size=20, face = "bold", hjust = 0.5, family = 'Roboto'))+
theme(axis.title.x     = element_text(size=20, color = "#FFFFFF", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.title.y     = element_text(size=20, color = "#FFFFFF", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.text.x      = element_text(size=25, color = "#FFFFFF", hjust = 0.5, vjust = 0.5,face = "bold", family = 'Roboto')) +
theme(axis.text.y      = element_text(size=25, color = "#FFFFFF", face = "bold", family = 'Roboto'))+
theme(legend.position = 'none')+
labs(title = "Pell Grants in US",
    subtitle = "Total number of recipients over the years",
    caption = "Data Source : Pell Awards/TidyTuesday/week 35 2022 \n Graphic Design : Deepali Kank")
