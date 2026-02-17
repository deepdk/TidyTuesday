# Libraries ----
library(tidyverse)
library(janitor)
library(scales)
library(glue)
library(here)
library(ggtext)
library(ggiraph)
library(showtext)
library(gt)
library(gtExtras)
library(reactable)
library(reactablefmtr)
library(dataui)
library(base64enc)

font_add_google("Outfit", "outfit")
font_add_google("DM Sans", "dm_sans")
showtext_auto()

# Load data ----
df <- tidytuesdayR::tt_load(2026, week = 7)$dataset


# Filter fruits ----
fruits <- c(
  "Apples", "Apricots", "Asian Pears (Nashi)", "Avocados",
  "Blackberries/Brambles", "Blackcurrants", "Blueberries", "Boysenberries",
  "Cherries", "Feijoas", "Grapefruit/Goldfruit", "Kiwifruit",
  "Lemons", "Lime", "Mandarins", "Melons (Water and Rock)",
  "Nectarines", "Oranges", "Passionfruit", "Peaches",
  "Pears (excluding Asian Pears)", "Persimmons", "Plums",
  "Raspberries", "Strawberries", "Tamarillos", "Tangelos"
)


# Fruit emoji mapping
fruit_icons <- c(
  "Apples" = "\U0001F34E",
  "Apricots" = "\U0001F351",
  "Asian Pears (Nashi)" = "\U0001F350",
  "Avocados" = "\U0001F951",
  "Blackberries/Brambles" = "\U0001FAD0",
  "Blackcurrants" = "\U0001F347",
  "Blueberries" = "\U0001FAD0",
  "Boysenberries" = "\U0001FAD0",
  "Cherries" = "\U0001F352",
  "Feijoas" = "\U0001F95D",
  "Grapefruit/Goldfruit" = "\U0001F34A",
  "Kiwifruit" = "\U0001F95D",
  "Lemons" = "\U0001F34B",
  "Lime" = "\U0001F34B",
  "Mandarins" = "\U0001F34A",
  "Melons (Water and Rock)" = "\U0001F348",
  "Nectarines" = "\U0001F351",
  "Oranges" = "\U0001F34A",
  "Passionfruit" = "\U0001F353",
  "Peaches" = "\U0001F351",
  "Pears (excluding Asian Pears)" = "\U0001F350",
  "Persimmons" = "\U0001F345",
  "Plums" = "\U0001F351",
  "Raspberries" = "\U0001FAD0",
  "Strawberries" = "\U0001F353",
  "Tamarillos" = "\U0001F345",
  "Tangelos" = "\U0001F34A"
)

# Clean up display names
fruit_display <- c(
  "Apples" = "Apples",
  "Apricots" = "Apricots",
  "Asian Pears (Nashi)" = "Asian Pears",
  "Avocados" = "Avocados",
  "Blackberries/Brambles" = "Blackberries",
  "Blackcurrants" = "Blackcurrants",
  "Blueberries" = "Blueberries",
  "Boysenberries" = "Boysenberries",
  "Cherries" = "Cherries",
  "Feijoas" = "Feijoas",
  "Grapefruit/Goldfruit" = "Grapefruit",
  "Kiwifruit" = "Kiwifruit",
  "Lemons" = "Lemons",
  "Lime" = "Lime",
  "Mandarins" = "Mandarins",
  "Melons (Water and Rock)" = "Melons",
  "Nectarines" = "Nectarines",
  "Oranges" = "Oranges",
  "Passionfruit" = "Passionfruit",
  "Peaches" = "Peaches",
  "Pears (excluding Asian Pears)" = "Pears",
  "Persimmons" = "Persimmons",
  "Plums" = "Plums",
  "Raspberries" = "Raspberries",
  "Strawberries" = "Strawberries",
  "Tamarillos" = "Tamarillos",
  "Tangelos" = "Tangelos"
)


df_react <- df |>
  filter(measure %in% fruits) |>
  group_by(measure) |>
  summarise(
    latest_value = value[which.max(year_ended_june)],
    unit = value_label[which.max(year_ended_june)],
    production = list(value),
    .groups = "drop"
  ) |>
  mutate(
    icon = fruit_icons[measure],
    fruit_name = fruit_display[measure],
    fruit_label = paste(icon, fruit_name)
  ) |>
  arrange(desc(latest_value)) |>
  select(fruit_label, latest_value, unit, production)

# Color palette
bg_cream <- "#faf7f2"
bg_header <- "#2d4a2d"
accent_green <- "#4a7c59"
accent_gold <- "#c8a951"
text_dark <- "#2c2c2c"
text_muted <- "#7a7a6e"
border_light <- "#e8e4dc"

rt <- reactable(
  df_react,
  theme = reactableTheme(
    style = list(
      fontFamily = "'Source Sans 3', sans-serif",
      background = bg_cream
    ),
    headerStyle = list(
      fontFamily = "'Fraunces', serif",
      fontSize = "13px",
      fontWeight = 600,
      textTransform = "uppercase",
      letterSpacing = "0.1em",
      color = text_muted,
      borderBottom = paste0("2px solid ", accent_green),
      paddingBottom = "12px",
      paddingTop = "12px",
      background = bg_cream
    ),
    cellStyle = list(
      paddingTop = "11px",
      paddingBottom = "11px",
      borderBottom = paste0("1px solid ", border_light)
    ),
    borderColor = "transparent"
  ),
  defaultColDef = colDef(
    vAlign = "center",
    headerVAlign = "bottom"
  ),
  columns = list(
    fruit_label = colDef(
      name = "",
      minWidth = 200,
      style = list(
        fontFamily = "'Source Sans 3', sans-serif",
        fontWeight = 600,
        fontSize = "18px",
        color = text_dark
      )
    ),
    latest_value = colDef(
      name = "Latest",
      minWidth = 110,
      align = "right",
      format = colFormat(separators = TRUE),
      style = list(
        fontFamily = "'Fraunces', serif",
        fontWeight = 500,
        fontSize = "17px",
        color = accent_green
      )
    ),
    unit = colDef(
      name = "Unit",
      minWidth = 120,
      style = list(
        fontSize = "14px",
        color = text_muted,
        fontStyle = "italic"
      )
    ),
    production = colDef(
      name = "Trend",
      minWidth = 260,
      cell = react_sparkline(
        df_react,
        height = 80,
        show_area = TRUE,
        area_color = accent_green,
        area_opacity = 0.15,
        line_color = accent_green,
        line_width = 1.8,
        line_curve = "cardinal",
        highlight_points = highlight_points(
          min = "#c0392b",
          max = accent_gold,
          last = accent_green
        ),
        point_size = 2.8,
        labels = c("min", "max", "last"),
        label_size = "0.65em",
        tooltip_type = 2,
        statline = "median",
        statline_color = accent_gold,
        statline_label_size = "0em"
      )
    )
  ),
  bordered = FALSE,
  compact = TRUE,
  fullWidth = FALSE,
  width = 800,
  defaultPageSize = 27
)

# Scattered fruit emoji decoration for the header
fruit_emojis <- paste0(
  "\U0001F34E \U0001F347 \U0001F352 \U0001F34A \U0001F95D ",
  "\U0001F353 \U0001F351 \U0001F350 \U0001F34B \U0001F348 ",
  "\U0001F951 \U0001FAD0"
)

# NZ map SVG from local file (Simplemaps, free for commercial use)
nz_svg_path <- here::here("nz.svg")
nz_svg_raw <- readLines(nz_svg_path, warn = FALSE) |> paste(collapse = "\n")

# Encode as base64 data URI for embedding in HTML
nz_svg_b64 <- base64enc::base64encode(charToRaw(nz_svg_raw))
nz_map_uri <- paste0("data:image/svg+xml;base64,", nz_svg_b64)

div_table <- htmltools::tagList(
  # Google Fonts
  htmltools::tags$head(
    htmltools::tags$link(
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Fraunces:opsz,wght@9..144,300;9..144,400;9..144,600;9..144,800",
        "&family=Source+Sans+3:wght@300;400;600",
        "&display=swap"
      ),
      rel = "stylesheet"
    )
  ),
  htmltools::div(
    style = paste0(
      "max-width: 860px; ",
      "background: ", bg_cream, "; ",
      "border-radius: 16px; ",
      "overflow: hidden; ",
      "box-shadow: 0 4px 24px rgba(0,0,0,0.08);"
    ),
    # Header banner
    htmltools::div(
      style = paste0(
        "background: ", bg_header, "; ",
        "padding: 36px 40px 32px 40px; ",
        "position: relative; ",
        "overflow: hidden;"
      ),
      # NZ map watermark
      htmltools::tags$img(
        src = nz_map_uri,
        style = paste0(
          "position: absolute; ",
          "right: 30px; top: 50%; ",
          "transform: translateY(-50%); ",
          "height: 160px; ",
          "opacity: 0.12; ",
          "filter: brightness(3); ",
          "pointer-events: none;"
        )
      ),
      # Fruit emoji strip
      htmltools::div(
        style = paste0(
          "font-size: 20px; ",
          "letter-spacing: 4px; ",
          "margin-bottom: 14px; ",
          "opacity: 0.7;"
        ),
        fruit_emojis
      ),
      # Title
      htmltools::div(
        style = paste0(
          "font-family: 'Fraunces', serif; ",
          "font-size: 34px; font-weight: 800; ",
          "color: #ffffff; ",
          "letter-spacing: -0.5px; ",
          "line-height: 1.15; ",
          "margin-bottom: 8px;"
        ),
        "New Zealand",
        htmltools::tags$br(),
        "Fruit Production"
      ),
      # Subtitle
      htmltools::div(
        style = paste0(
          "font-family: 'Source Sans 3', sans-serif; ",
          "font-size: 14px; ",
          "color: rgba(255,255,255,0.6); ",
          "font-weight: 300;"
        ),
        htmltools::HTML(glue(
          "Agricultural production trends across ",
          "<span style='color:{accent_gold}; font-weight:600;'>{nrow(df_react)} fruits</span>",
          " &middot; {year_min}\u2013{year_max}"
        ))
      )
    ),
    # Table body
    htmltools::div(
      style = "padding: 0 28px;",
      rt
    ),
    # Footer
    htmltools::div(
      style = paste0(
        "font-family: 'Source Sans 3', sans-serif; ",
        "font-size: 11px; color: ", text_muted, "; ",
        "padding: 16px 40px 20px 40px; ",
        "border-top: 1px solid ", border_light, "; ",
        "margin: 0 28px;"
      ),
      htmltools::HTML(
        "Source: StatsNZ via Figure.NZ &middot; TidyTuesday 2026 Week 7 &middot; Viz: Deepali Kank"
      )
    )
  )
)

htmltools::browsable(div_table)

htmltools::save_html(div_table, file.path(plots_dir, "tt_2026_w07_fruit_table_react.html"))

