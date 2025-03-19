library(tidyverse)
library(scales)
library(janitor)
library(showtext)
library(ggtext)
library(MetBrewer)


palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


palm_data <- palmtrees |> 
  filter(!is.na(max_leaf_number)) |> 
  group_by(palm_tribe) |> 
  summarise(avg = mean(max_leaf_number)) |> 
  arrange(desc(avg)) 



# 1. Function to create a single curved leaf:
create_leaf <- function(angle, length = 3, n = 100, curve = 1) {
  # Parameter t goes from 0 to 1
  t <- seq(0, 1, length.out = n)
  
  # A simple quadratic curve for the leaf
  x <- t * length * cos(angle) - curve * t^2 * sin(angle)
  y <- t * length * sin(angle) + curve * t^2 * cos(angle)
  
  data.frame(x = x, y = y)
}

# 2. Create trunk data for each tribe:
#    Here, the trunk is always drawn from y=0 to y=4.
all_trunks <- palm_data %>%
  rowwise() %>%
  do({
    tibble(
      palm_tribe = .$palm_tribe,
      x = 0,
      y = seq(0, 4, length.out = 100)  # 100 points up the trunk
    )
  }) %>%
  ungroup()

# 3. Create leaf data for each tribe:
#    - The number of leaves = avg (rounded down or up as integer).
#    - We distribute angles in a half-circle, or any pattern you prefer.
all_leaves <- palm_data %>%
  rowwise() %>%
  do({
    n_leaves <- max(1, round(.$avg))  # ensure at least 1 leaf
    # Example: distribute leaves from pi/8 to 7*pi/8
    angles <- seq(pi/8, 7*pi/8, length.out = n_leaves)
    
    # Create a leaf data frame for each angle, then bind them together
    leaf_dfs <- lapply(angles, function(a) {
      df_leaf <- create_leaf(angle = a, length = 3, n = 100, curve = 1)
      # Shift the leaf up so it starts at the trunk top (y=4)
      df_leaf$y <- df_leaf$y + 4
      # Label each leaf so we can group them properly
      df_leaf$leaf_id <- paste0("leaf_", round(a, 2))
      df_leaf
    }) %>%
      bind_rows()
    
    # Attach the tribe name to each row
    leaf_dfs$palm_tribe <- .$palm_tribe
    leaf_dfs
  }) %>%
  ungroup()

# 4. Plot the data:
#    - Draw trunks with geom_path
#    - Draw leaves with geom_line
#    - Facet by palm_tribe
p <- ggplot() +
  # Trunks
  geom_path(
    data = all_trunks,
    aes(x = x, y = y, group = palm_tribe),
    color = "sienna4", size = 2, lineend = "round"
  ) +
  # Leaves
  geom_line(
    data = all_leaves,
    aes(x = x, y = y, group = interaction(palm_tribe, leaf_id)),
    color = "forestgreen", size = 0.5
  ) +
  # Facet one palm tribe per panel
  facet_wrap(~ palm_tribe) +
  # Fix aspect ratio and remove background/axes
  coord_fixed() +
  theme_void() 
 

width_px = 700
height_px = 500
dpi = 96 
width_in_inches = width_px / dpi
height_in_inches = height_px / dpi
svglite("p.svg", width = width_in_inches, height = height_in_inches)
print(p) # Plot output is captured and written to the file
dev.off()
