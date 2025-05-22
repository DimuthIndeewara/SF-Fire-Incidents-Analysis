library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(fresh)
library(dplyr)
library(lubridate)
library(shinyBS)
library(leaflet)
library(ggplot2)

# Set working directory and load data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data <- read.csv("Corrected Fire Data Analysis.csv", header = TRUE)

# Convert date and datetime columns
date_columns <- c("Call.Date", "Watch.Date")
data[date_columns] <- lapply(data[date_columns], as.Date, format = "%Y-%m-%d")

data <- data %>%
  filter(!(year(Call.Date) == 2024 & month(Call.Date) == 6)) # Remove incomplete data for June 2024

data <- data %>%
  filter(year(Call.Date) == 2023 )

datetime_columns <- c("Received.DtTm", "Response.DtTm", "Hospital.DtTm", "Entry.DtTm", "On.Scene.DtTm", "Dispatch.DtTm", 
                      "Transport.DtTm","Available.DtTm","data_as_of", "data_loaded_at" )
data[datetime_columns] <- lapply(data[datetime_columns], as.POSIXct, format = "%Y-%m-%d %H:%M:%S")

# Define UI for the dashboard

my_theme <- create_theme(
  adminlte_color(
    light_blue = "#470D21"
  ),
  adminlte_sidebar(
    dark_bg = "#34495e",
    dark_hover_bg = "#1abc9c",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#ecf0f1",
    box_bg = "#ffffff",
    info_box_bg = "#ffffff"
  )
)

ui <- dashboardPage(
  dashboardHeader(
    title = "",
    tags$li(actionButton("filterBtn", "Filters", icon = icon("filter")), class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt"))
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tags$head(
      tags$style(HTML("
        .info-box {
          min-height: 70px;
        }
        .info-box-icon {
          height: 70px; 
          line-height: 70px;
        }
        .info-box-content {
          padding-top: 0;
          padding-bottom: 0;
        }
        .info-box-text {
          white-space: normal;
          font-size: 14px;
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 6,
                       fluidRow(
                         box(width = 6, title = "Fire Incidents", status = "primary", solidHeader = TRUE,
                             infoBoxOutput("fireIncidents", width = 8)),
                         box(width = 6, title = "Number of Stations", status = "primary", solidHeader = TRUE,
                             infoBoxOutput("numStations", width = 8))
                       ),
                       fluidRow(
                         box(width = 6, title = "New Card 1", status = "primary", solidHeader = TRUE,
                             infoBoxOutput("newCard1", width = 8)),
                         box(width = 6, title = "New Card 2", status = "primary", solidHeader = TRUE,
                             infoBoxOutput("newCard2", width = 8))
                       )
                ),
                column(width = 6,
                       box(width = 12, title = "San Francisco Map", status = "primary", solidHeader = TRUE,
                           leafletOutput("sfMap", height = 350))
                )
              ),
              fluidRow(
                box(width = 6, title = "Incidents by Month", status = "primary", solidHeader = TRUE,
                    plotOutput("incidentsByMonth")),
                box(width = 6, title = "Top 10 Fire Incidents by Call Type", status = "primary", solidHeader = TRUE,
                    plotOutput("incidentsByCallType"))
              )
      )
    ),
    bsModal("filterModal", "Filter Options", "filterBtn", size = "large",
            fluidRow(
              column(width = 12,
                     dateRangeInput("dateRange", "Select Date Range:",
                                    start = min(data$Call.Date, na.rm = TRUE),
                                    end = max(data$Call.Date, na.rm = TRUE),
                                    min = min(data$Call.Date, na.rm = TRUE),
                                    max = max(data$Call.Date, na.rm = TRUE),
                                    separator = " - ",
                                    format = "yyyy-mm-dd",
                                    startview = "month",
                                    language = "en"
                     ),
                     selectInput("year", "Select Year:", choices = c("All", unique(year(data$Call.Date))), selected = "All"),
                     selectInput("callType", "Call Type:", choices = c("All", unique(data$Call.Type)), selected = "All"),
                     selectInput("stationArea", "Station Area:", choices = c("All", unique(data$Station.Area)), selected = "All"),
                     selectInput("battalion", "Battalion:", choices = c("All", unique(data$Battalion)), selected = "All"),
                     selectInput("supervisorDistrict", "Supervisor District:", choices = c("All", unique(data$Supervisor.District)), selected = "All")
              )
            )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected date range, call type, station area, and year
  filtered_data <- reactive({
    req(input$dateRange)
    data_filtered <- data %>%
      filter(Call.Date >= input$dateRange[1] & Call.Date <= input$dateRange[2])
    
    if (!is.null(input$year) && input$year != "All") {
      data_filtered <- data_filtered %>% filter(year(Call.Date) == as.numeric(input$year))
    }
    
    if (input$callType != "All") {
      data_filtered <- data_filtered %>% filter(Call.Type == input$callType)
    }
    
    if (input$stationArea != "All") {
      data_filtered <- data_filtered %>% filter(Station.Area == input$stationArea)
    }
    
    if (input$battalion != "All") {
      data_filtered <- data_filtered %>% filter(Battalion == input$battalion)
    }
    
    if (input$supervisorDistrict != "All") {
      data_filtered <- data_filtered %>% filter(Supervisor.District == input$supervisorDistrict)
    }
    
    data_filtered
  })
  
  # Calculate the number of fire incidents based on selected filters
  fire_incidents <- reactive({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  # Calculate the number of unique stations based on selected filters
  num_stations <- reactive({
    req(filtered_data())
    n_distinct(filtered_data()$Station.Area)
  })
  
  # Render the infoBox with the number of fire incidents
  output$fireIncidents <- renderInfoBox({
    infoBox(
      "Total Fire Incidents", fire_incidents(), icon = icon("fire"),
      color = "red"
    )
  })
  
  # Render the infoBox with the number of unique stations
  output$numStations <- renderInfoBox({
    infoBox(
      "Number of Stations", num_stations(), icon = icon("building"),
      color = "blue"
    )
  })
  
  # Plot showing incidents by month
  output$incidentsByMonth <- renderPlot({
    req(filtered_data())
    filtered_data() %>%
      mutate(month = format(Call.Date, "%Y-%m")) %>%
      count(month) %>%
      ggplot(aes(x = month, y = n)) +
      geom_bar(stat = "identity", fill = "#9DB2BF") +
      labs(x = "Month", y = "Number of Incidents") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Plot showing incidents by call type
  output$incidentsByCallType <- renderPlot({
    req(filtered_data())
    filtered_data() %>%
      count(Call.Type) %>%
      top_n(10, n) %>%
      ggplot(aes(x = reorder(Call.Type, n), y = n, fill = Call.Type)) +
      geom_bar(stat = "identity", fill = "#9DB2BF") +
      labs(x = "Call Type", y = "Number of Incidents") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "none")
  })
  
  output$sfMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -122.4194, lat = 37.7749, zoom = 11.5)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
