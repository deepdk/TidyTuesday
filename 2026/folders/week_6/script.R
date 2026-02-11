# Libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(here)
library(ggtext)
library(ggiraph)
library(showtext)

font_add_google("Outfit", "outfit")
font_add_google("DM Sans", "dm_sans")
showtext_auto()

source(here::here('R', 'theme_deepali.R'))
theme_set(theme_deepali())

# Load data ----
df <- tidytuesdayR::tt_load(2026, week = 6)$schedule


df |>
  count(discipline_name, sort = TRUE)

df |>
  ggplot(aes(date, discipline_name)) +
  geom_point(aes(color = discipline_name)) +
  theme_minimal() +
  theme(legend.position = "none")


# Radial bar chart: Marathon Days vs. Rest Days ----

df_daily <- df %>%
  group_by(date) %>%
  summarize(
    total_events = n(),
    medal_events = sum(is_medal_event),
    training_events = sum(is_training),
    other_events = total_events - medal_events - training_events
  ) %>%
  ungroup() %>%
  mutate(
    day_num = row_number(),
    day_label = format(date, "%b %d"),
    is_peak = total_events == max(total_events),
    is_quietest = total_events == min(total_events)
  )

# Pivot to long for stacked radial bars
df_long <- df_daily %>%
  select(
    date,
    day_num,
    day_label,
    is_peak,
    is_quietest,
    medal_events,
    training_events,
    other_events
  ) %>%
  pivot_longer(
    cols = c(medal_events, training_events, other_events),
    names_to = "event_type",
    values_to = "count"
  ) %>%
  mutate(
    event_type = factor(
      event_type,
      levels = c("medal_events", "other_events", "training_events"),
      labels = c("Medal Events", "Other Events", "Training")
    )
  )

# Colors
col_medal <- "#FFD700"
col_other <- "#66FCF1"
col_training <- "#C5C6C7"

# Annotation data
peak_day <- df_daily %>% filter(is_peak) %>% slice(1)
quiet_day <- df_daily %>% filter(is_quietest) %>% slice(1)

p <- ggplot(df_long, aes(x = day_num, y = count, fill = event_type)) +
  geom_col(width = 0.75) +
  coord_polar(start = 0) +
  scale_fill_manual(
    values = c(
      "Medal Events" = col_medal,
      "Other Events" = col_other,
      "Training" = col_training
    )
  ) +
  scale_x_continuous(
    breaks = df_daily$day_num,
    labels = df_daily$day_label,
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.05))) +
  # Peak day annotation
  annotate(
    "text",
    x = peak_day$day_num,
    y = peak_day$total_events + 8,
    label = glue("Peak\n{peak_day$total_events} events"),
    color = col_medal,
    size = 5,
    fontface = "bold",
    family = "outfit"
  ) +
  # Quietest day annotation
  annotate(
    "text",
    x = quiet_day$day_num,
    y = quiet_day$total_events + 12,
    label = glue("Quietest\n{quiet_day$total_events} events"),
    color = col_training,
    size = 5,
    fontface = "bold",
    family = "outfit"
  ) +
  labs(
    title = "THE MARATHON DAYS VS. REST DAYS",
    subtitle = glue(
      "Daily event density across the Olympics — ",
      "<span style='color:{col_medal};'>**medal events**</span>, ",
      "<span style='color:{col_training};'>**training sessions**</span>, ",
      "and <span style='color:{col_other};'>**everything in between**</span>"
    ),
    caption = "Source: TidyTuesday 2026 Week 6 | Viz: Deepali Kank",
    fill = NULL
  ) +
  theme_deepali(base_size = 30, base_family = "outfit") +
  theme(
    axis.text.x = element_text(
      size = 22,
      colour = "grey70",
      family = "dm_sans"
    ),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(colour = "grey20", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      family = "outfit",
      face = "bold",
      size = 45,
      hjust = 0.5,
      margin = margin(b = 8)
    ),
    plot.subtitle = element_markdown(
      family = "dm_sans",
      size = 25,
      colour = "grey70",
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    plot.caption = element_text(
      family = "dm_sans",
      size = 14,
      colour = "grey50",
      hjust = 0.5,
      margin = margin(t = 15)
    ),
    legend.position = "none",
    plot.margin = margin(30, 30, 30, 30)
  )

p

ggsave(
  file.path(plots_dir, "tt_2026_w06_marathon_days.png"),
  plot = p,
  width = 9,
  height = 10,
  dpi = 300,
  bg = "#0b0c10"
)


# Radial Flow Calendar: The Olympic Day Journey ----

df_radial <- df %>%
  filter(!is.na(start_datetime_local), !is.na(end_datetime_local)) %>%
  distinct(event_code, start_datetime_local, .keep_all = TRUE) %>%
  mutate(
    event_date = as.Date(start_datetime_local),
    start_h = hour(start_datetime_local) + minute(start_datetime_local) / 60,
    end_h = hour(end_datetime_local) + minute(end_datetime_local) / 60,
    end_h = if_else(end_h <= start_h, 24, end_h), # handle midnight crossover
    day_num = as.numeric(event_date - min(event_date)) + 1,
    day_label = format(event_date, "%b %d")
  )

n_days <- max(df_radial$day_num)

# Highlight only Ice Hockey
df_radial <- df_radial %>%
  mutate(
    disc_highlight = if_else(
      discipline_name == "Ice Hockey",
      "Ice Hockey",
      "Other"
    )
  )

disc_palette <- c("Ice Hockey" = "#66FCF1", "Other" = "grey18")

# Day ring labels — placed at the start of each ring
day_labels <- df_radial %>%
  group_by(day_num, day_label) %>%
  summarize(first_hour = min(start_h), .groups = "drop")

# Medal event markers (placed at midpoint of each medal event arc)
df_medals <- df_radial %>%
  filter(is_medal_event) %>%
  mutate(mid_h = (start_h + end_h) / 2)

p2 <- ggplot(df_radial) +
  # Event arcs — all on the same level
  geom_rect(
    aes(
      xmin = start_h,
      xmax = end_h,
      ymin = day_num - 0.4,
      ymax = day_num + 0.4,
      fill = disc_highlight
    ),
  ) +
  # Medal event markers
  # geom_point(
  #   data = df_medals,
  #   aes(x = mid_h, y = day_num),
  #   color = "#4593C3", fill = "#4593C3",
  #   shape = 23, size = 1, stroke = 0.5
  # ) +
  # Day labels at the top — only first and last day
  geom_text(
    data = day_labels %>%
      filter(day_num == min(day_num) | day_num == max(day_num)),
    aes(x = 0.0, y = day_num, label = day_label),
    color = "white",
    size = 7,
    hjust = 0.5,
    vjust = 0.5,
    family = "dm_sans"
  ) +
  coord_polar(start = 0) +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 23, by = 3),
    labels = paste0(seq(0, 23, by = 3), ":00"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-2, n_days + 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = disc_palette, breaks = "Ice Hockey") +
  labs(
    title = "<span style='color:#66FCF1;'>ICE HOCKEY</span> AT MILANO CORTINA 2026",
    subtitle = "Each ring is a day of the Games — Ice Hockey events highlighted across the full schedule",
    caption = "Source: TidyTuesday 2026 Week 6 | Viz: Deepali Kank",
    fill = NULL
  ) +
  theme_deepali(base_size = 40, base_family = "outfit") +
  theme(
    axis.text.x = element_text(
      size = 28,
      colour = "grey60",
      family = "dm_sans"
    ),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(colour = "grey15", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_markdown(
      family = "outfit",
      face = "bold",
      size = 64,
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "dm_sans",
      size = 32,
      colour = "grey70",
      hjust = 0.5,
      margin = margin(b = 25)
    ),
    plot.caption = element_text(
      family = "dm_sans",
      size = 22,
      colour = "grey50",
      hjust = 0.5,
      margin = margin(t = 20)
    ),
    legend.position = "none",
    legend.text = element_text(family = "dm_sans", size = 22),
    legend.title = element_text(family = "outfit", size = 26, face = "bold"),
    plot.margin = margin(40, 40, 40, 40)
  ) +
  guides(fill = guide_legend(ncol = 1, override.aes = list(alpha = 1)))


ggsave(
  file.path(plots_dir, "tt_2026_w06_radial_calendar.png"),
  plot = p2,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "#0b0c10"
)


# Small Multiples: Radial Flow Calendar per Sport ----

# Get top disciplines by number of events (to keep facets manageable)
top_disciplines <- df_radial %>%
  count(discipline_name, sort = TRUE) %>%
  #filter(n >= 10) %>%
  pull(discipline_name)

# Build facet data: duplicate all events for each focal discipline
df_facet <- df_radial %>%
  filter(discipline_name %in% top_disciplines) %>%
  select(event_code, start_h, end_h, day_num, day_label, discipline_name) %>%
  crossing(focal = top_disciplines) %>%
  mutate(
    is_focal = discipline_name == focal,
    focal = factor(focal, levels = top_disciplines)
  )

# Background layer: all events in muted grey (draw once per facet)
df_bg <- df_radial %>%
  select(event_code, start_h, end_h, day_num) %>%
  crossing(focal = factor(top_disciplines, levels = top_disciplines))

# Foreground: only the focal sport's events
df_fg <- df_facet %>% filter(is_focal)

# Single highlight color for all sports — icy blue
highlight_col <- "#4FC3F7"
sport_colors <- setNames(
  rep(highlight_col, length(top_disciplines)),
  top_disciplines
)

# Day labels for first/last day only
day_labels_facet <- df_radial %>%
  group_by(day_num, day_label) %>%
  summarize(.groups = "drop") %>%
  filter(day_num == min(day_num) | day_num == max(day_num)) %>%
  crossing(focal = factor(top_disciplines, levels = top_disciplines))

p3 <- ggplot() +
  # Background: all events in muted slate blue
  geom_rect(
    data = df_bg,
    aes(
      xmin = start_h,
      xmax = end_h,
      ymin = day_num - 0.4,
      ymax = day_num + 0.4
    ),
    fill = "#dde6ef"
  ) +
  # Foreground: highlighted sport
  geom_rect(
    data = df_fg,
    aes(
      xmin = start_h,
      xmax = end_h,
      ymin = day_num - 0.4,
      ymax = day_num + 0.4,
      fill = discipline_name
    )
  ) +
  # Day labels
  geom_text(
    data = day_labels_facet,
    aes(x = 0, y = day_num, label = day_label),
    color = "#5A7A9A",
    size = 4.5,
    family = "dm_sans"
  ) +
  coord_polar(start = 0) +
  scale_x_continuous(
    limits = c(0, 24),
    breaks = seq(0, 23, by = 6),
    labels = paste0(seq(0, 23, by = 6), ":00"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-2, n_days + 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = sport_colors) +
  facet_wrap(~focal, ncol = 4) +
  labs(
    title = "WHEN EACH SPORT TAKES THE STAGE",
    subtitle = "Radial calendar of Milano Cortina 2026 — each panel highlights one discipline across all days of the Games",
    caption = "Source: TidyTuesday 2026 Week 6 | Viz: Deepali Kank",
    fill = NULL
  ) +
  theme_minimal(base_size = 30, base_family = "outfit") +
  theme(
    plot.background = element_rect(fill = "#E8F0F8", colour = NA),
    panel.background = element_rect(fill = "#E8F0F8", colour = NA),
    axis.text.x = element_text(
      size = 20,
      colour = "#5A7A9A",
      family = "dm_sans"
    ),
    axis.text.y = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(colour = "#C8D8E8", linewidth = 0.15),
    panel.grid.minor = element_blank(),
    strip.text = element_text(
      family = "outfit",
      face = "bold",
      size = 40,
      colour = "#1B3A5C",
      margin = margin(b = 5, t = 5)
    ),
    plot.title = element_text(
      family = "outfit",
      face = "bold",
      size = 80,
      colour = "#1B3A5C",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      family = "dm_sans",
      size = 50,
      colour = "#4A6A8A",
      hjust = 0.5,
      margin = margin(b = 25)
    ),
    plot.caption = element_text(
      family = "dm_sans",
      size = 22,
      colour = "#7A9AB0",
      hjust = 0.5,
      margin = margin(t = 20)
    ),
    legend.position = "none",
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(30, 20, 30, 20)
  )

p3

ggsave(
  file.path(plots_dir, "tt_2026_w06_radial_small_multiples.png"),
  plot = p3,
  width = 16,
  height = 16,
  dpi = 300,
  bg = "#E8F0F8"
)
