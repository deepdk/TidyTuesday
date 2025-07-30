# Load libraries
library(tidyverse)
library(showtext)
library(janitor)
library(scales)
library(lubridate)
library(geofacet)
library(glue)
library(ggtext)

# Font setup
font_add_google("Cabin", "cabin")
showtext_auto()

# Define constants
country_exclude <- c("Puerto Rico", "Ireland", "England", "Canada", "Italy", "Germany", "Mexico", "Cuba", "Scotland", 
                     "Japan", "Russia", "Romania", "Vietnam", "Asia Minor", "Austria", "Bermuda", "China", "Denmark", 
                     "France", "Hong Kong", "Hungary", "India", "Latvia", "Poland", "Spain", "Venezuela", "Antigua", 
                     "Australia", "Austria-Hungary", "Brazil", "Colombia", "Columbia", "Dominican Republic", "Ecuador",
                     "Jamaica", "Norway", "Prussia", "Saudi Arabia", "Sierra Leone", "Sweden", "Syria", "Ukraine")

parties <- c("Democratic", "Republican")
party_colors <- c("Democratic" = "#2E74C0", "Republican" = "#CB454A")

# Text elements
title_text <- "Where U.S. Federal Judges Were Born?"
subtitle_text <- glue(
  "Many federal judges that were nominated during 1789-2014 were born in a handful of populous states such as ",
  "New York, Pennsylvania, and Illinois. ",
  "The chart displays their birthplaces by state, colored by the political ",
  "party of the nominating president. ",
  "<span style='color:{party_colors[\"Democratic\"]};'>Democratic</span> and ",
  "<span style='color:{party_colors[\"Republican\"]};'>Republican</span>."
)
caption_text <- "Data: {historydata} R package · Graphic: Deepali Kank"

# Read data
judges_appointments <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_appointments.csv")
judges_people <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-10/judges_people.csv")

# Join and clean
df <- left_join(judges_appointments, judges_people, by = "judge_id") |>
  filter(
    !is.na(birthplace_state),
    president_party %in% parties,
    !birthplace_state %in% country_exclude
  ) |> 
  mutate(
    birthplace_state = str_replace(birthplace_state, "VA \\(now WV\\)", "WV")
  ) |> 
  count(birthplace_state, president_party, name = "n")

# Plot
ggplot(df, aes(x = president_party, y = n, fill = president_party)) +
  geom_col(width = 0.7) +
  coord_flip() +
  facet_geo(~birthplace_state, grid = "us_state_grid3") +
  scale_fill_manual(values = party_colors) +
  scale_y_continuous(
    breaks = c(0, 250),
    limits = c(0, 250),
    expand = expansion(mult = c(0, 0.02))
  ) +
  theme_minimal(base_size = 14, base_family = "cabin") +
  theme(
    axis.title         = element_blank(),
    axis.text.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid         = element_blank(),
    legend.position    = "none",
    strip.text         = element_text(size = 12),
    strip.background   = element_rect(fill = "#e9ecef", colour = "#e9ecef", linewidth = 0.25),
    plot.title.position = "plot",
    plot.title         = element_textbox_simple(margin = margin(b = 10), size = 40, face = "bold"),
    plot.subtitle      = element_textbox_simple(margin = margin(b = 15), size = 20, lineheight = 0.6),
    plot.caption       = element_textbox_simple(margin = margin(t = 10), size = 10, hjust = 0.5, family = "barlow"),
    plot.margin        = margin(10, 10, 10, 10)
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  )
