library(tidyverse)
library(wordcloud2)
library(ggwordcloud)
library(wordcloud)
library(ggstream)
library(svglite)
library(tidytext)

report_words_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-25/report_words_clean.csv')

bing_sentiments <- get_sentiments("bing")

sentiment_data <- report_words_clean |>
  inner_join(bing_sentiments, by = "word")

yearly_sentiment <- sentiment_data |>
  count(year, sentiment) |>
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  )

p2 <- yearly_sentiment |>
  pivot_longer(
    cols = c(positive, negative),
    names_to = "sentiment",
    values_to = "count"
  ) |>
  ggplot(aes(x = year, y = count, fill = sentiment)) +
  #geom_area() + 
  geom_stream(type = "proportional", bw=1) +
  scale_fill_manual(values = c("positive" = "#297373", "negative" = "#EDA39C")) +
  scale_y_continuous(
    limits = c(0, NA),  # Start from 0, upper limit auto-calculated
    expand = expansion(mult = c(0, 0.05))  # Optional: tighter axis spacing
  ) +
  labs(
    title = "Positive vs Negative Sentiment in Amazon Annual Reports",
    x = "Year",
    y = "Word Count",
    color = "Sentiment"
  ) +
  theme_minimal()

width_px = 800
height_px = 500
dpi = 96 
width_in_inches = width_px / dpi
height_in_inches = height_px / dpi
svglite("p2.svg", width = width_in_inches, height = height_in_inches)
print(p2) # Plot output is captured and written to the file
dev.off()

# Word cloud is generated using online wordcloud generator and placed above the stream chart using Figma.
