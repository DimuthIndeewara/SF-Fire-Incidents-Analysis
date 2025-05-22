library(bs4Dash)
library(readxl)
library(shiny)
library(tidyverse)
library(highcharter)
library(xts)
library(DT)
library(shinyWidgets)
library(waiter)
library(shinycssloaders)
library(zip)
library(leaflet)
library(sf)
library(leaflet.extras)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# -----------------------------------------------------------------------------------------------------
data <- read.csv("Corrected Fire Data Analysis.csv", header = TRUE)

fire <- read.csv("Fire_Incidents.csv", header = TRUE)

fire_station <- read.csv("City Fire Station Data.csv", header = TRUE)

fire_station <- fire_station %>%
  filter(jurisdiction == "Fire Department" )

supervisor_district <- read_excel("Supervisor_Districts.xlsx")

# -----------------------------------------------------------------------------------------------------

data <- merge(data, fire, by = "Incident.Number")

data["Total_Injuries"] =  data$Fire.Injuries + data$Civilian.Injuries
data["Total_Fatalities"] =  data$Fire.Fatalities + data$Civilian.Fatalities

data$Supervisor.District <- as.numeric(data$Supervisor.District)

# Convert date and datetime columns
date_columns <- c("Call.Date", "Watch.Date")
data[date_columns] <- lapply(data[date_columns], as.Date, format = "%Y-%m-%d")

data <- data %>%
  filter(!(year(Call.Date) == 2024 & month(Call.Date) == 6)) # Remove incomplete data for June 2024

data <- data %>%
  filter(year(Call.Date) == 2023 )

datetime_columns <- c("Received.DtTm", "Response.DtTm", "Hospital.DtTm", "Entry.DtTm", "On.Scene.DtTm", "Dispatch.DtTm", 
                      "Transport.DtTm","Available.DtTm","data_as_of", "data_loaded_at")

data[datetime_columns] <- lapply(data[datetime_columns], as.POSIXct, format = "%Y-%m-%d %H:%M:%S")

data <- data %>%
  mutate(Dispatch_Time = as.numeric(difftime(Dispatch.DtTm, Received.DtTm, units = "mins")),
         Response_Time = as.numeric(difftime(Response.DtTm, Dispatch.DtTm, units = "mins")),
         Travel_Time = as.numeric(difftime(On.Scene.DtTm, Response.DtTm, units = "mins")),
         Travel_Time_to_Hospital = as.numeric(difftime(Hospital.DtTm, Transport.DtTm, units = "mins")),
         Total_Response_Time = as.numeric(difftime(On.Scene.DtTm, Received.DtTm, units = "mins")),
         Total_occupied_Time = as.numeric(difftime(Available.DtTm, Received.DtTm, units = "mins")))

data <- data %>%
  mutate(Category = case_when(
    Call.Final.Disposition %in% c("Code 2 Transport", "Code 3 Transport", 
                                  "Multi-casualty Incident", "Against Medical Advice", 
                                  "Patient Declined Transport", "Medical Examiner", 
                                  "Gone on Arrival", "Unable to Locate" , "Multi-casualty Incident") ~ "EMS",
    Call.Final.Disposition %in% c("Fire", "No Merit", "Cancelled") ~ "Fire",
    Call.Final.Disposition %in% c("Duplicate", "Other","SFPD", "CHP") ~ "Other",
    TRUE ~ "Unknown"  # Catch-all for any unexpected values
  ))

# --------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  # preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#18191A"),
  fullscreen = TRUE,
  dashboardHeader(title = dashboardBrand(
    title = "SF Fire Dispatched Dashboard",
    color = "gray-dark" ),
  .list = list(
    tags$style(HTML("
        #call_type_filter_ui, #dates_filter_ui, #station_area_filter_ui, #supervisor_district_filter_ui {
          width: 190px; /* Adjust the width as needed */
          display: inline-block;
        }
      ")),
    uiOutput("dates_filter_ui"),
    tags$pre(" "),
    uiOutput("call_type_filter_ui"),
    tags$pre(" "),
    uiOutput("station_area_filter_ui"),
    tags$pre(" "),
    uiOutput("supervisor_district_filter_ui"),
    tags$pre(" "),
    downloadBttn(
      outputId = "patient_admission_report_download",
      label = "Download",
      style = "unite",
      size = "sm"
    )
    # downloadBttn(
    #   outputId = "patient_admission_bulk_report_download",
    #   label = "Download Multiple Reports",
    #   style = "unite",
    #   size = "sm"
    # )
  ),
  titleWidth = 500), # end of header
  dashboardSidebar(disable = TRUE), # end of Sidebar
  dashboardBody(
    fluidPage(
      # zoom out the dashboard page to 90%
      tags$style(HTML("
      body {
      zoom: 90%;
    }
  ")),
      fluidRow(
        # column(5,
        #        box(title = "Top 5 diagnoses by admissions", status = "white", solidHeader = TRUE,
        #            width = 16, icon = icon("chart-bar"), collapsible = FALSE,
        #            withSpinner(highchartOutput("top_5_diagnosis_by_admission", height = 200)))),
        column(4,
               infoBoxOutput("number_of_incident", width = 12),
               infoBoxOutput("number_of_injuries", width = 12),
               infoBoxOutput("number_of_fatalities", width = 12)),
        column(4,
               infoBoxOutput("number_of_stations", width = 12),
               infoBoxOutput("number_of_district", width = 12),
               infoBoxOutput("number_of_bat", width = 12)),
        column(4,
               infoBoxOutput("average_dispatchT", width = 12),
               infoBoxOutput("average_occupT", width = 12),
               infoBoxOutput("average_responseT", width = 12))
      ),
      
      fluidRow(
        column(6,
               box(title = "Daily Fire Incidents", status = "gray-dark", solidHeader = TRUE,
                   width = 12, icon = icon("chart-line"), collapsible = FALSE,
                   withSpinner(highchartOutput("fire_trend", height = 400))),
               
               fluidRow(
                 column(6,
                        box(title = "Top 5 Fire Incidents", status = "gray-dark", solidHeader = TRUE,
                            width = 12, icon = icon("chart-bar"), collapsible = FALSE,
                            withSpinner(highchartOutput("top_5_fire_incidents", height = 300)))
                 ),
                 column(6,
                        box(title = "Fire Incidents by Call Type", status = "gray-dark", solidHeader = TRUE,
                            width = 12, icon = icon("chart-pie"), collapsible = FALSE,
                            withSpinner(highchartOutput("top_3_fire_category", height = 300)))
                 )
               )
        ),
        column(6,
               box(title = "Fire Incidents Map", status = "gray-dark", solidHeader = TRUE,
                   width = 12, icon = icon("map"), collapsible = FALSE,
                   withSpinner(leafletOutput("sfMap", height = 800)))
        )
      ),
      fluidRow(
        column(12,
               box(title = "Fire Incident Data",
                   status = "gray-dark", solidHeader = TRUE, width = 12,
                   icon = icon("table"),
                   maximizable = FALSE, collapsible = TRUE,
                   DTOutput("fire_incident_datatable"))
        )
      )
    )
  )
) # end of dashboard page


# ----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$dates_filter_ui <- renderUI({
    dateRangeInput("dateRange", "Select Incident Period", 
                   start = min(data$Call.Date, na.rm = TRUE), 
                   end = max(data$Call.Date, na.rm = TRUE),
                   min = min(data$Call.Date, na.rm = TRUE),
                   max = max(data$Call.Date, na.rm = TRUE))
  })
  
  output$call_type_filter_ui <- renderUI({
    selectInput("callType", "Select Incident Type", choices = c("All", sort(unique(data$Call.Type))))
  })
  
  output$station_area_filter_ui <- renderUI({
    selectInput("stationArea", "Station Area", choices = c("All", sort(unique(data$Station.Area))))
  })
  
  output$supervisor_district_filter_ui <- renderUI({
    selectInput("supervisorDistrict", "Supervisor District", choices = c("All", sort(unique(data$Supervisor.District))))
  })

  
  filtered_data <- reactive({
    req(input$dateRange)
    data_filtered <- data %>%
      filter(Call.Date >= input$dateRange[1] & Call.Date <= input$dateRange[2])
    
    if (input$callType != "All") {
      data_filtered <- data_filtered %>% filter(Call.Type == input$callType)
    }
    
    if (input$stationArea != "All") {
      data_filtered <- data_filtered %>% filter(Station.Area == input$stationArea)
    }
    
    if (input$supervisorDistrict != "All") {
      data_filtered <- data_filtered %>% filter(Supervisor.District == input$supervisorDistrict)
    }
    
    data_filtered
  })
  
  
  fire_incidents <- reactive({
    req(filtered_data())
    nrow(filtered_data())
  })
  
  output$number_of_incident <- renderInfoBox({
    infoBox(
      title = "Number of Incidents",
      value = fire_incidents(),
      icon = icon("fire"),
      color = "maroon"
    )
  })
  
  
  
  # Calculate the number of person injured
  total_injuries <- reactive({
    req(filtered_data())
    sum(filtered_data()$Total_Injuries)
  })
  
  output$number_of_injuries <- renderInfoBox({
    infoBox(
      title = "Number of Injuries",
      value = total_injuries(),
      icon = icon("hospital"),
      color = "maroon"
    )
  })
  
  # Calculate the number of person dead
  total_dead <- reactive({
    req(filtered_data())
    sum(filtered_data()$Total_Fatalities)
  })
  
  output$number_of_fatalities <- renderInfoBox({
    infoBox(
      title = "Number of Fatalities",
      value = total_dead(),
      icon = icon("person"),
      color = "maroon"
    )
  })
  
  # Calculate the number of fire station
  total_station <- reactive({
    req(filtered_data())
    n_distinct(filtered_data()$Station.Area)
  })
  
  output$number_of_stations <- renderInfoBox({
    infoBox(
      title = "Number of Stations",
      value = total_station(),
      icon = icon("location-crosshairs"),
      color = "lightblue"
    )
  })
  
  # Calculate the number of supervisor district
  total_districts <- reactive({
    req(filtered_data())
    n_distinct(filtered_data()$Supervisor.District)
  })
  
  output$number_of_district <- renderInfoBox({
    infoBox(
      title = "Number of Supervisor Districts",
      value = total_districts(),
      icon = icon("map"),
      color = "lightblue"
    )
  })
  
  # Calculate the number of Batalion
  total_bat <- reactive({
    req(filtered_data())
    n_distinct(filtered_data()$Battalion)
  })
  
  output$number_of_bat <- renderInfoBox({
    infoBox(
      title = "Number of Battalions",
      value = total_bat(),
      icon = icon("user-group"),
      color = "lightblue"
    )
  })
  
  # Calculate the average dispatch time
  avg_dis <- reactive({
    req(filtered_data())
    round(mean(filtered_data()$Dispatch_Time, na.rm = TRUE),2)
  })
  
  output$average_dispatchT <- renderInfoBox({
    infoBox(
      title = "Average Dispatch Time (min)",
      value = avg_dis(),
      icon = icon("truck-field"),
      color = "olive"
    )
  })
  
  # Calculate the average response time
  avg_response <- reactive({
    req(filtered_data())
    round(mean(filtered_data()$Total_Response_Time, na.rm = TRUE),2)
  })
  
  output$average_responseT <- renderInfoBox({
    infoBox(
      title = "Average Response Time (min)",
      value = avg_response(),
      icon = icon("reply"),
      color = "olive"
    )
  })
  
  # Calculate the average occupied time
  avg_occupied <- reactive({
    req(filtered_data())
    round(mean(filtered_data()$Total_occupied_Time, na.rm = TRUE),2)
  })
  
  output$average_occupT <- renderInfoBox({
    infoBox(
      title = "Average Occupied Time (min)",
      value = avg_occupied(),
      icon = icon("clock"),
      color = "olive"
    )
  })
  
  
  # Daily Fire Incident 
  fire_trend <- reactive({
    req(filtered_data())
    filtered_data() %>%
      group_by(Call.Date) %>%
      summarise(fire = n()) %>%
      arrange(Call.Date)
  })
  
  
  # Output for daily fire incidents trend
  output$fire_trend <- renderHighchart({
    data_fire <- fire_trend()
    
    # Convert the data to xts format
    data_fire_xts <- xts(x = data_fire$fire, order.by = data_fire$Call.Date)
    
    # Create the Highchart
    highchart(type = "stock") %>%
      hc_add_series(data_fire_xts, 
                    type = "line", 
                    color = "#FF5733", 
                    name = "Fire Incidents") %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Number of Incidents"), 
               opposite = FALSE) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme(chart = list(backgroundColor = "")))
  })
  
  
  # Top 5 Fire Categories
  
  fire_category <- reactive({
    req(filtered_data())
    filtered_data() %>%
      group_by(Call.Type) %>%
      summarise(incidents = n()) %>%
      arrange(desc(incidents))
  })
  
  output$top_5_fire_incidents <- renderHighchart({
    # Ensure the data has at least one row
    req(nrow(fire_category()) > 0)
    
    fire_category() %>%
      head(5) %>%
      hchart(
        "bar",
        hcaes(x = Call.Type, y = incidents),
        color = "#005383",
        dataLabels = list(enabled = TRUE, format = "{y}"),
        name = "Incidents"
      ) %>%
      hc_title(text = "") %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Number of Incidents"), labels = list(format = "{value}")) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(
        hc_theme(chart = list(
          backgroundColor = ""
        ))
      )
  })
  
  # Fire Priorities
  fire_category_call <- reactive({
    req(filtered_data())
    filtered_data() %>%
      group_by(Category) %>%
      summarise(fincidents = n()) %>%
      arrange(desc(fincidents))
  })
  
  output$top_3_fire_category <- renderHighchart({
    # Ensure the data has at least one row
    req(nrow(fire_category_call()) > 0)
    
    fire_category_call() %>%
      head(3) %>%
      hchart(
        "pie",
        hcaes(name = Category, y = fincidents),
        color = c("maroon", "#009E49", "#998E73"),  # Custom colors for segments
        dataLabels = list(enabled = TRUE, format = "{point.name}: {point.y}"),
        name = "Incidents"
      ) %>%
      hc_title(text = "") %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(
        hc_theme(chart = list(
          backgroundColor = ""
        ))
      )
  })
  
  
  
  
  output$sfMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Light Theme") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark Theme") %>%
      
      setView(lng = -122.4194, lat = 37.7749, zoom = 12.47) %>%
      
      addCircleMarkers(data = fire_station,
                       ~longitude, ~latitude,
                       radius = 5,
                       color = "#8B0000",
                       fillColor = "#FF0000",
                       fillOpacity = 0.8,
                       popup = ~common_name,
                       group = "Fire Stations") %>% 
      
      addLayersControl(
        baseGroups = c("Light Theme", "Dark Theme"),
        overlayGroups = c("Fire Stations","Coverage Areas", "Fire Incidents"),
        options = layersControlOptions(collapsed = TRUE) 
      ) %>% 
      
      hideGroup("Coverage Areas") %>% 
      hideGroup("Fire Incidents")
      
      
  })
  
  observe({
    leafletProxy("sfMap") %>%
      addCircleMarkers(data = fire_station,
                       ~longitude, ~latitude,
                       radius = 5,
                       color = "#8B0000",
                       fillColor = "#FF0000",
                       fillOpacity = 0.8,
                       popup = ~common_name,
                       group = "Fire Stations") %>%
      
      addCircles(data = fire_station,
                 lng = ~longitude,
                 lat = ~latitude,
                 radius = 1500,  # Radius in meters (adjust as needed)
                 color = "#00008B",
                 weight = 1,
                 opacity = 0.5,
                 fillColor = "#ADD8E6",
                 fillOpacity = 0.2,
                 group = "Coverage Areas") %>% 
      
      addCircleMarkers(data = filtered_data(),
                       ~lon, ~lat,
                       radius = 5,
                       color = "#FF4500",
                       fillColor = "#FF6347",
                       fillOpacity = 0.8,
                       #popup = ~description,
                       group = "Fire Incidents")
    
    
  
    
    
  })
  observe({
    req(filtered_data())  # Ensure filtered_data is available
    
    leafletProxy("sfMap") %>%
      clearMarkers() %>%
      addCircleMarkers(data = fire_station,
                       ~longitude, ~latitude,
                       radius = 5,
                       color = "#8B0000",
                       fillColor = "#FF0000",
                       fillOpacity = 0.8,
                       popup = ~common_name,
                       group = "Fire Stations") %>%
      
      addCircles(data = fire_station,
                 lng = ~longitude,
                 lat = ~latitude,
                 radius = 1500,  # Radius in meters (adjust as needed)
                 color = "#00008B",
                 weight = 1,
                 opacity = 0.5,
                 fillColor = "#ADD8E6",
                 fillOpacity = 0.2,
                 group = "Coverage Areas") %>% 
      
      addCircleMarkers(data = filtered_data(),
                       ~lon, ~lat,
                       radius = 5,
                       color = "#FF4500",
                       fillColor = "#FF6347",
                       fillOpacity = 0.8,
                       group = "Fire Incidents")
  })
  
  output$fire_incident_datatable <- renderDT({
    data <- filtered_data()  
    req(nrow(data) > 0)
    
    DT::datatable(
      data |> arrange(desc(Call.Date)),  # Sort by Call.Date in descending order
      rownames = FALSE,
      extensions = 'Buttons',
      filter = "top",
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        info = TRUE,
        dom = 'Blfrtip',
        buttons = c('csv', 'excel', 'pdf', 'print'),
        lengthMenu = list(
          c(5, 10, 30, 50, -1),
          c('5', '10', '30', '50', 'All')
        ),
        paging = TRUE
      ),
      colnames = c(
        "Incident Number" = "Incident.Number",
        "Call Date" = "Call.Date",
        "Call Type" = "Call.Type",
        "Dispatch Time" = "Dispatch.DtTm",
        "Response Time" = "Response.DtTm"
      )
    )
  })
  
  colnames(data)
  
}

shinyApp(ui = ui, server = server)


