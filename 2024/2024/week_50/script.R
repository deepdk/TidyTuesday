# Load necessary libraries
library(tidyverse)  # For data manipulation and visualization
library(scales)     # For scaling values
library(lubridate)  # For handling dates
library(janitor)    # For cleaning column names

# Import the cleaned Parfumo dataset
parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv') |>
  clean_names()  # Clean column names for easier manipulation

# Preview the dataset
view(parfumo_data_clean)

# Analyze brand and perfume counts
parfumo_data_clean |>
  count(brand, sort = TRUE)  # 1442 brands available in the dataset

parfumo_data_clean |>
  count(name, sort = TRUE)  # 55,110 unique perfumes

# Focus on top notes
# Select the "top_notes" column and separate the notes into individual entries
df <- parfumo_data_clean |>
  select(top_notes) |>
  mutate(top_notes = str_split(top_notes, ", ")) %>%  # Split multiple notes into a list
  unnest_wider(top_notes, names_sep = "_")  # Expand the list into separate columns

# Transform the data to a long format
df <- df |>
  pivot_longer(cols = c(1:25),  # Transform all top notes columns into rows
               names_to = "name",  # New column for original column names
               values_to = "note_name")  # New column for the actual note names

# Filter out missing values and count occurrences of each note
df <- df |>
  filter(!is.na(note_name)) |>  # Remove rows with missing values
  count(note_name, sort = TRUE)  # Count each note and sort by frequency

# Preview the resulting dataset
view(df)

# Summary:
# There are a total of 2,430 unique top notes in the dataset.
