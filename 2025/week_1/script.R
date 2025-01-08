library(bskyr)
library(tidyverse)
library(showtext)
library(tidyr)

auth <- bs_auth(user = user_name, pass = password)


actor <- "deepalikank.in"

feed <- bs_get_author_feed(
  actor,
  cursor = NULL,
  limit = 1000,
  user = user_name,
  pass = password
  auth = auth,
  clean = TRUE
)

view(feed)

feed_short <- feed |> 
  select(uri,author_handle,author_display_name,reply_count,like_count,repost_count,quote_count,indexed_at)

# Using separate() to split the column into date and time
feed_short <- feed_short %>%
  separate(indexed_at, into = c("date", "time"), sep = "T") %>%
  mutate(time = str_remove(time, "Z"))  

my <-  feed_short |> 
  filter(author_handle == "deepalikank.in")

view(my)

sum(my$like_count)

sum(my$reply_count)

sum(my$repost_count)
