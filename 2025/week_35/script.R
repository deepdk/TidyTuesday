library(tidyverse)
library(sf)
library(janitor)
library(showtext)
library(ggfx)
library(lubridate)
library(sysfonts)
library(ggtext)
library(glue)
library(grid)
library(patchwork)


sysfonts::font_add_google("Inter", "Inter", regular.wt = 400, bold.wt = 700)
showtext::showtext_auto()


frogID_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frogID_data.csv") |>
  clean_names()
frog_names <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-02/frog_names.csv") |>
  clean_names()

frogID_data <- frogID_data |>
  mutate(
    event_date = as.Date(event_date),
    year = year(event_date),
    month = month(event_date),
    day = day(event_date)
  )


season_of <- function(m) {
  dplyr::case_when(
    m %in% c(12, 1, 2) ~ "Summer",
    m %in% 3:5 ~ "Autumn",
    m %in% 6:8 ~ "Winter",
    m %in% 9:11 ~ "Spring",
    TRUE ~ NA_character_
  )
}

frogs_2023 <- frogID_data %>%
  filter(year == 2023) %>%
  filter(!is.na(decimal_longitude), !is.na(decimal_latitude)) %>%
  mutate(season = factor(season_of(month),
    levels = c("Summer", "Autumn", "Winter", "Spring")
  )) %>%
  filter(!is.na(season))


frogs_sf <- st_as_sf(
  frogs_2023,
  coords = c("decimal_longitude", "decimal_latitude"),
  crs = 4326, remove = FALSE
) %>% st_transform(3577) # EPSG:3577

aus_states <- ozmaps::ozmap_states %>% st_transform(3577)


season_cols <- c(
  Summer = "#540d6e",
  Autumn = "#ee4266",
  Winter = "#ffd23f",
  Spring = "#3bceac"
)



theme_frogmap <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = 12, margin = margin(6, 0, 6, 0), family = "Inter"),
      strip.background = element_rect(fill = "#f4f4f4", color = NA),
      panel.spacing = unit(12, "pt"),
      plot.margin = margin(5, 5, 5, 5),
      legend.position = "none"
    )
}


title_html <- "<span style='font-weight:900; font-size:40px'>When frogs show up in Australia </span>"

subtitle_md <- glue(
  "Southern hemisphere seasons • ",
  "<b style='color:{season_cols['Summer']}'>Summer</b>, ",
  "<b style='color:{season_cols['Autumn']}'>Autumn</b>, ",
  "<b style='color:{season_cols['Winter']}'>Winter</b>, ",
  "<b style='color:{season_cols['Spring']}'>Spring</b> • ",
  "Summer includes Jan–Feb and Dec 2023<br/><br/>",
  "<span style='color:#333'>Hotspots hug the wetter east; ",
  "<b>Spring</b> and <b>Autumn</b> show the broadest spread, while ",
  "<b>Winter</b> contracts toward the south.</span><br/><br/>",
  "<span style='color:#222'><b>Context.</b> Australia is home to ",
  "<b>257</b> native frog species, yet almost <b>1 in 5</b> are threatened with extinction due to ",
  "climate change, urbanisation, disease, and invasive species.</span>"
)


LEFT_WIDTH <- 0.36 # same as before


p_text <- ggplot() +
  annotate("rect",
    xmin = 0, xmax = 1, ymin = 0, ymax = 1,
    fill = "white", color = NA
  ) +
  ggtext::geom_textbox(
    aes(x = 0.06, y = 0.92, label = title_html),
    width = unit(0.88, "npc"),
    box.color = NA, fill = NA,
    hjust = 0, vjust = 1, halign = 0,
    family = "Inter", size = 10, lineheight = 1.05
  ) +
  ggtext::geom_textbox(
    aes(x = 0.06, y = 0.70, label = subtitle_md),
    width = unit(0.88, "npc"),
    box.color = NA, fill = NA,
    hjust = 0, vjust = 1, halign = 0,
    family = "Inter", size = 4.5, lineheight = 1.24
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "on") +
  theme_void(base_family = "Inter") +
  theme(plot.margin = margin(16, 6, 16, 16))


p_maps <- ggplot() +
  geom_sf(data = aus_states, fill = "grey98", color = "grey70", linewidth = 0.2) +
  geom_sf(
    data = frogs_sf, aes(color = season),
    pch = 16, size = 0.85, alpha = 0.35, show.legend = FALSE
  ) +
  scale_color_manual(values = season_cols, drop = FALSE) +
  coord_sf(expand = FALSE) +
  facet_wrap(~season, ncol = 2) +
  theme_frogmap()

combined <- cowplot::plot_grid(
  p_text, p_maps,
  ncol = 2,
  rel_widths = c(LEFT_WIDTH, 1 - LEFT_WIDTH),
  align = "h", axis = "t"
)

final_plot <- cowplot::ggdraw(combined) +
  cowplot::draw_label(
    "Data: frogid.net.au  •  Graphic: Deepali Kank",
    x = 1, y = 0.05, hjust = 2, vjust = 0,
    fontfamily = "Inter", size = 10, color = "grey40"
  )

final_plot
