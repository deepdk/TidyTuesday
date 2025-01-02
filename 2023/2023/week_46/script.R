library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(shinydashboard)
library(MetBrewer)
library(shinythemes)
library(fresh)


diwali_sales_data <- read.csv("D:/TidyTuesday_2023/diwali_sales.csv")



# Define the User Interface
# Define the User Interface using shinydashboard components
ui <- dashboardPage(
    dashboardHeader(title = "Diwali Sales Dashboard"),
    dashboardSidebar(
        selectInput("marital_status", "Marital Status:", 
                    choices = c("All", "Married", "Unmarried")),
        selectInput("occupation", "Occupation:",
                    choices = c("All", unique(diwali_sales_data$occupation)))
    ),
    dashboardBody(
      includeCSS("www/style.css"),
        fluidRow(
            valueBoxOutput("totalSales"),
            valueBoxOutput("totalOrders"), 
            valueBoxOutput("totalCustomers"),
        ),
      fluidRow(
        box(
          title = "Diwali Sales in Indain Retail Store", width = 12, solidHeader = TRUE, status = "primary",
          "The data this week comes from sales data for a retail store during the Diwali festival period in India. The data is shared on Kaggle by Saad Haroon."
       
      ),
        fluidRow(
            box(title = "Sales Analysis", solidHeader = TRUE,
                tabsetPanel(
                    tabPanel("Top Product Categories by Sales", plotOutput("topProductPlot")),
                    tabPanel("Sales by State", plotOutput("salesByStatePlot")),
                    tabPanel("Sales by Age Group", plotOutput("salesByAgeGroupPlot")),
                    
                    id = "tabs"
                ),
                width = 12
            )
        )
        
    )
)
)


# Define the Server Logic
server <- function(input, output) {
    
    
    # Reactive expression for filtered data
    filteredData <- reactive({
        data <- diwali_sales_data
        if(input$marital_status != "All") {
            Marital_Status <- ifelse(input$marital_status == "Married", 1, 0)
            data <- data[data$marital_status == Marital_Status, ]
        }
        if(input$occupation != "All") {
            data <- data[data$occupation == input$occupation, ]
        }
        data
    })
    
    # Total Sales KPI
    output$totalSales <- renderValueBox({
        valueBox(
            paste0(12,79,254),  # Replace with actual sales calculation
            "Total Sales",
            icon = icon("dollar"),
            color = "light-blue"
        )
    })
    
    # Total Orders KPI
    output$totalOrders <- renderValueBox({
        valueBox(
            27981,  # Replace with actual orders calculation
            "Total Orders",
            icon = icon("shopping-cart"),
            color = "light-blue"
        )
    })
    
    output$totalCustomers <- renderValueBox({
        valueBox(
            11251,  # Replace with actual orders calculation
            "Total Customers",
            icon = icon("users"),
            color = "light-blue"
        )
    })
    
    
    # Plot for top product categories by sales
    output$topProductPlot <- renderPlot({
        data <- filteredData()
        data %>%
            filter(!is.na(amount)) |> 
            group_by(product_category, gender) %>%
            summarise(Total_Sales = sum(amount)) %>%
            ggplot(aes(x = product_category, y = Total_Sales, fill = gender)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_y_continuous(labels = comma) +
            scale_fill_manual(values = c("#607B8B","#87CEFA")) +
            #facet_wrap(~gender) +
            theme_minimal() +
            theme(legend.position = "top") + 
            theme(axis.text.x = element_text(angle = 45)) +
            theme(axis.title.x = element_blank()) 
    })
    
    # Plot for sales by state
    output$salesByStatePlot <- renderPlot({
        data <- filteredData()
        data %>%
            filter(!is.na(amount)) |> 
            group_by(state, gender) %>%
            summarise(Total_Sales = sum(amount)) %>%
            ggplot(aes(x = state, y = Total_Sales, fill = gender)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_y_continuous(labels = comma) +
            scale_fill_manual(values = c("#607B8B","#87CEFA")) +
            #facet_wrap(~gender) +
            theme_minimal() +
            theme(legend.position = "top") +
            theme(axis.text.x = element_text(angle = 45)) +
            theme(axis.title.x = element_blank())
    })
    
    # Plot for sales by occupation
    output$salesByAgeGroupPlot <- renderPlot({
        data <- filteredData()
        data %>%
            group_by(age_group, gender) %>%
            summarise(Total_Sales = sum(amount)) %>%
            ggplot(aes(x = age_group, y = Total_Sales, fill = gender)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_y_continuous(labels = comma) +
            scale_fill_manual(values = c("#607B8B","#87CEFA")) +
            #facet_wrap(~gender) +
            theme_minimal() +
            theme(legend.position = "top") + 
            theme(axis.text.x = element_text(angle = 45)) +
            theme(axis.title.x = element_blank()) 
        
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
