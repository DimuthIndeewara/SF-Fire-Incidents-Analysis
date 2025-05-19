library(sf)
library(ggmap)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(dplyr)
library(ggmap)
library(patchwork)
library(viridis)
library(sp)
library(leaflet.extras)
library(readxl)
library(osrm)
library(leaflet)
library(purrr)
library(MASS)
library(geosphere)

# --------------------------------------------------------------------------------------------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ============================================== Data ==========================================================

data=read.csv("Corrected Fire Data Analysis.csv", header = TRUE)

data_fire_station <- read_excel("City Fire Station Data.xlsx")

data_fire_station <- data_fire_station %>% 
  filter(jurisdiction == "Fire Department")

data$Call.Date=as.Date(data$Call.Date,format="%Y-%m-%d")

filter.data=data%>%
  filter(Call.Date>=as.Date("2020-01-01")&Call.Date<=("2024-01-01"))

head(filter.data)

# ---------------------------------------------------------------------

fire_incidents_filter=filter.data%>%
  filter(Call.Type %in% c("Outside Fire","Structure Fire / Smoke in Building","Alarms"))%>%
  mutate(Year=floor_date(Call.Date,"year"))
  # filter(year(Year)==2020)

fire_incidents=fire_incidents_filter%>%
  group_by(lon,lat)%>%
  summarise(Fire_Alarm_Count=n())

pal <- colorNumeric(palette = brewer.pal(12, "YlOrRd"), domain = fire_incidents$Fire_Alarm_Count)

# Create the leaflet map
leaflet(data = fire_incidents) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = ~lon,
    lat = ~lat,
    weight = 1,
    radius = ~Fire_Alarm_Count,  # Adjust the multiplier to scale circle sizes
    color = ~pal(Fire_Alarm_Count),
    fillOpacity = 1
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~Fire_Alarm_Count,
    title = "Fire Alarm Count"
  ) %>%
  setView(lng = mean(fire_incidents$lon), lat = mean(fire_incidents$lat), 
          zoom = 11.8)

kde <- kde2d(filter.data$lon, filter.data$lat, n = 100)

# Find the location with maximum density
max_density_index <- which.max(kde$z)
max_density_row <- max_density_index %% nrow(kde$z)
max_density_col <- max_density_index %/% nrow(kde$z) + 1

densest_lon <- kde$x[max_density_row]
densest_lat <- kde$y[max_density_col]

# Print the result
cat("The densest area is approximately at:\n")
cat("Longitude:", densest_lon, "\n")
cat("Latitude:", densest_lat, "\n")

# ----------------------------------------------------------------

leaflet(data = fire_incidents) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~Fire_Alarm_Count,
    blur = 10,
    max = max(fire_incidents$Fire_Alarm_Count, na.rm = TRUE),
    radius = 20
  ) %>%
  addCircleMarkers(
    data = data_fire_station,  # Your fire stations data frame
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,  # Size of the dot
    color = 'black',  # Color of the dot
    fillOpacity = 1,  # Full opacity
    stroke = FALSE,  # No border
    popup = ~facility_id,  # Popup with facility_id
    label = ~common_name,  # Label with facility_id
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      textOnly = FALSE  # Display only the text (no marker icon)
      #direction = 'auto'  # Automatically adjust label direction
    )
  ) %>%
  setView(lng = mean(fire_incidents$lon), lat = mean(fire_incidents$lat), 
          zoom = 11.8)

# ----------------------------------------------------------------------
leaflet(data = fire_incidents) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~Fire_Alarm_Count,
    blur = 10,
    max = max(fire_incidents$Fire_Alarm_Count, na.rm = TRUE),
    radius = 15
  ) %>%
  addCircleMarkers(
    data = data_fire_station,  # Your fire stations data frame
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,  # Size of the dot
    color = 'black',  # Color of the dot
    fillOpacity = 1,  # Full opacity
    stroke = FALSE,  # No border
    popup = ~facility_id,  # Popup with facility_id
    label = ~common_name,  # Label with facility_id
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      textOnly = FALSE  # Display only the text (no marker icon)
      #direction = 'auto'  # Automatically adjust label direction
    )
  ) %>%
  addCircleMarkers(
    lng = densest_lon,  # Longitude of the densest area
    lat = densest_lat,  # Latitude of the densest area
    radius = 3,  # Size of the dot
    color = 'blue',  # Color of the dot
    fillOpacity = 1,  # Full opacity
    stroke = FALSE  # No border
  ) %>%
  addLabelOnlyMarkers(
    lng = densest_lon,  # Longitude of the densest area
    lat = densest_lat,  # Latitude of the densest area
    label = "Densest Area",  # Label text
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      direction = 'top',  # Place the label above the point
      textOnly = TRUE,  # Display only the text (no marker icon)
      style = list('color' = 'blue')  # Color of the text
    )
  ) %>%
  setView(lng = mean(fire_incidents$lon), lat = mean(fire_incidents$lat), zoom = 11.8)

# ---------------------------------------------------------

densest_location <- c(densest_lon, densest_lat)
fire_stations_coords <- data_fire_station[, c("longitude", "latitude")]
distances <- distHaversine(fire_stations_coords, densest_location)

data_fire_station$distance_to_dense <- distances

closest_fire_stations <- data_fire_station[order(data_fire_station$distance_to_dense), ][1:5, ]


# ---------------------------------------------------------

leaflet(data = fire_incidents) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~Fire_Alarm_Count,
    blur = 10,
    max = max(fire_incidents$Fire_Alarm_Count, na.rm = TRUE),
    radius = 20
  ) %>%
  addCircleMarkers(
    data = closest_fire_stations,  # Your fire stations data frame
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,  # Size of the dot
    color = 'black',  # Color of the dot
    fillOpacity = 1,  # Full opacity
    stroke = FALSE,  # No border
    popup = ~facility_id,  # Popup with facility_id
    label = ~common_name,  # Label with facility_id
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      textOnly = FALSE , # Display only the text (no marker icon)
      direction = 'top'  # Automatically adjust label direction
    )
  ) %>%
  addCircleMarkers(
    lng = densest_lon,  # Longitude of the densest area
    lat = densest_lat,  # Latitude of the densest area
    radius = 3,  # Size of the dot
    color = 'blue',  # Color of the dot
    fillOpacity = 1,  # Full opacity
    stroke = FALSE  # No border
  ) %>%
  addLabelOnlyMarkers(
    lng = densest_lon,  # Longitude of the densest area
    lat = densest_lat,  # Latitude of the densest area
    label = "",  # Label text
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      direction = 'top',  # Place the label above the point
      textOnly = TRUE,  # Display only the text (no marker icon)
      style = list('color' = '')  # Color of the text
    )
  ) %>%
  setView(lng = mean(fire_incidents$lon), lat = mean(fire_incidents$lat), zoom = 11.8)


# ---------------------------------------------------------------
locations <- tibble::tribble(
  ~station,     ~lon,      ~lat,
  #"02",  -122.4099, 37.79703,
  #"41",  -122.4165, 37.79338,
  "03",  -122.4193, 37.78664,
  #"36",  -122.4212, 37.77493,
  #"01",  -122.4041, 37.77946
  
)


isochrone <- map2(locations$lon, locations$lat, 
                  ~ osrmIsochrone(loc = c(.x, .y),
                                  breaks = seq(0, 5, .5))) %>%
  do.call(what = rbind)

iso <- isochrone %>% 
  mutate(drive_times = factor(paste(isomin, "to", isomax, "min")))


# factpal <- colorFactor("YlOrRd", iso$drive_times)
factpal <- colorFactor("Greys", iso$drive_times)



p <- leaflet(data = fire_incidents) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    lng = ~lon,
    lat = ~lat,
    intensity = ~Fire_Alarm_Count,
    blur = 10,
    max = max(fire_incidents$Fire_Alarm_Count, na.rm = TRUE),
    radius = 15
  ) %>%
  setView(lng = mean(locations$lon), lat = mean(locations$lat), zoom = 13) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Greyscale") %>%
  addPolygons(
    data = iso,
    fill = TRUE,
    stroke = TRUE,
    color = "black",
    fillColor = ~factpal(iso$drive_times),
    weight = 0.5,
    fillOpacity = 0.8,
    popup = ~iso$drive_times,
    group = "Drive Time"
  ) %>%
  addMarkers(
    lng = locations$lon,
    lat = locations$lat,
    popup = locations$station,
    label = locations$station,  # Label with facility_id
    labelOptions = labelOptions(
      noHide = TRUE,  # Keep the label visible
      textOnly = FALSE ,
      direction = 'auto'
    
  )) %>%
  addLegend(
    position = "bottomright",
    pal = factpal,
    values = iso$drive_times,
    title = "Drive Time"
  )

p






