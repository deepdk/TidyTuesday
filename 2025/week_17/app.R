library(shiny)
library(tidyverse)
library(lubridate)

# Load and prepare data
schedule <- read.csv("useR.csv", stringsAsFactors = FALSE)
# Explicitly parse dates in MM/DD/YYYY format
schedule$date <- mdy(schedule$date)

available_dates <- unique(schedule$date)

ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # Custom Conference Header
    tags$div(HTML('
        <div class="conference-header">
          <h1>useR! 2025</h1>
          <div class="event-types">
            <div class="event">
              <span class="icon">💻</span>
              <span>Virtual</span>
              <div class="event-date">1 August 2025</div>
            </div>
            <div class="event">
              <span class="icon">👥</span>
              <span>In-person</span>
              <div class="event-date">8–10 August 2025</div>
            </div>
          </div>
          <div class="location">
            Penn Pavilion, Duke University<br>
            Durham, North Carolina
          </div>
          <div class="hosted-by">
            Hosted by Duke University and Duke Center for Computational Thinking
          </div>
        </div>
    ')),
    
    # Filter section
    tags$div(class = "filter-section",
             # Both filters in one row
             tags$div(class = "filter-row",
                      # Session Type Filter
                      tags$div(class = "session-type-filter",
                               selectInput("session_type", "Session Type", choices = c("All", unique(schedule$session)))
                      ),
                      
                      # Date Picker Buttons - Correctly display August dates
                      tags$div(class = "date-picker-row",
                               lapply(available_dates, function(d) {
                                   # Format as "Aug 1", "Aug 8", etc.
                                   date_label <- format(d, "%b %d")
                                   actionButton(
                                       inputId = paste0("date_", d), 
                                       label = date_label, 
                                       class = "date-button"
                                   )
                               }),
                               actionButton("reset_dates", "All Dates", class = "date-button reset-button")
                      ),
             )
    ),
    
    # Main content - Only the gallery
    tags$div(class = "main-content",
             uiOutput("gallery_ui"),
             uiOutput("modal_ui")
    )
)

server <- function(input, output, session) {
    selected_date <- reactiveVal(NULL)
    
    # Watch for date button clicks
    observe({
        lapply(available_dates, function(d) {
            observeEvent(input[[paste0("date_", d)]], {
                selected_date(d)
            })
        })
        observeEvent(input$reset_dates, {
            selected_date(NULL)
        })
    })
    
    filtered_data <- reactive({
        df <- schedule
        if (input$session_type != "All") {
            df <- df %>% filter(session == input$session_type)
        }
        if (!is.null(selected_date())) {
            df <- df %>% filter(date == selected_date())
        }
        df
    })
    
    output$gallery_ui <- renderUI({
        sessions <- filtered_data()
        if (nrow(sessions) == 0) return(tags$p("No sessions match the selected criteria."))
        
        cards <- lapply(1:nrow(sessions), function(i) {
            row <- sessions[i, ]
            
            # Add session-specific class for styling
            card_class <- paste("session-card", tolower(row$session), sep=" ")
            
            tags$div(class = card_class,
                     tags$div(class = "card-title", row$title),
                     tags$div(class = "card-meta", paste(row$speakers, "|", row$time, "|", row$room)),
                     tags$div(class = "card-tags", 
                              tags$span(class = "tag", row$session)
                     ),
                     actionButton(inputId = paste0("open_", i), label = "View Details", class = "view-button")
            )
        })
        
        # Create observer for modal display
        lapply(seq_len(nrow(sessions)), function(i) {
            observeEvent(input[[paste0("open_", i)]], {
                showModal(modalDialog(
                    title = sessions[i, "title"],
                    tags$p(sessions[i, "content"]),
                    easyClose = TRUE,
                    footer = modalButton("Close")
                ))
            })
        })
        
        tags$div(class = "card-gallery", cards)
    })
    
    output$modal_ui <- renderUI({ NULL })
}

shinyApp(ui = ui, server = server)