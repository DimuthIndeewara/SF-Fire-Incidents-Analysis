#install.packages("sf")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("scales")) install.packages("scales")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("sf")) install.packages("sf")
if (!require("ggmap")) install.packages("ggmap")


library(sf)
library(ggmap)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)
library(RColorBrewer)

# Load necessary libraries
library(tidyverse)
library(sf) # for spatial data
library(spdep) # for spatial regression
library(spatialreg) # for spatial regression

#-----------------Working Directory---------------------------------------
# Set the working directory
setwd("/Users/chiaravonwatzdorf/Desktop/Fire Data")

# Read the data
fire_data_all <- read.csv("Corrected Fire Data Analysis.csv", header = TRUE)

#crime_data <- read.csv("Police_Department_Incident_Reports__2018_to_Present_20240709.csv", Header = TRUE)

crime_data <- read.csv("Police_Department_Incident_Reports__2018_to_Present_20240709.csv", header = TRUE)

# Preview the data
head(fire_data_all)
head(crime_data)

#------------------ combine data sets ------------------

# Aggregate fire incidents by neighborhood and date
incident_data_sf <- fire_data_all %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Call.Date`) %>%
  summarize(incident_count = n(), .groups = 'drop')

# Aggregate crime incidents by neighborhood and date
crime_data_sf <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Date`) %>%
  summarize(crime_count = n(), .groups = 'drop') %>%
  filter(!is.na(`Analysis.Neighborhood`) & `Analysis.Neighborhood` != "") 

# Rename columns to make them consistent for merging
incident_data_sf <- fire_data_all %>%
  rename(Neighborhood = `Neighborhooods...Analysis.Boundaries`,
         Date = `Call.Date`)

crime_data_sf <- crime_data %>%
  rename(Neighborhood = `Analysis.Neighborhood`,
         Date = `Incident.Date`)

# Merge the two data sets
merged_incident_data <- merge(incident_data_sf, crime_data_sf, by = c("Neighborhood", "Date"), all = TRUE)

# Display the merged data
head(merged_incident_data)

#--------------- refined code ------------------------------------------------------------------------------------


# Step 1: Aggregate fire incidents by neighborhood and date
incident_data_sf <- fire_data_all %>%
  group_by(Neighborhooods...Analysis.Boundaries, Call.Date) %>%
  summarize(incident_count = n(), .groups = 'drop')

# Step 2: Aggregate crime incidents by neighborhood and date
crime_data_sf <- crime_data %>%
  group_by(Analysis.Neighborhood, Incident.Date) %>%
  summarize(crime_count = n(), .groups = 'drop') %>%
  filter(!is.na(Analysis.Neighborhood) & Analysis.Neighborhood != "") 

# Step 3: Rename columns to make them consistent for merging
incident_data_sf <- incident_data_sf %>%
  rename(Neighborhood = Neighborhooods...Analysis.Boundaries,
         Date = Call.Date)

crime_data_sf <- crime_data_sf %>%
  rename(Neighborhood = Analysis.Neighborhood,
         Date = Incident.Date)

head(crime_data_sf)

# Convert the Date column in crime_data_sf to Date format
crime_data_sf$Date <- as.Date(crime_data_sf$Date, format = "%Y/%m/%d")

# Convert the Date column in incident_data_sf to Date format (if not already done)
incident_data_sf$Date <- as.Date(incident_data_sf$Date, format = "%Y-%m-%d")

# ----------- merge the data sets --------------------------------------------------
# Merge the two data sets on Neighborhood and Date
merged_incident_data <- merge(incident_data_sf, crime_data_sf, by = c("Neighborhood", "Date"), all = TRUE)

# Display the first few rows of the merged data
head(merged_incident_data)

# Check the structure of the merged data
str(merged_incident_data)

# Ensure incident_count and crime_count are numeric
merged_incident_data$incident_count <- as.numeric(merged_incident_data$incident_count)
merged_incident_data$crime_count <- as.numeric(merged_incident_data$crime_count)



# Filter out neighborhoods with insufficient data and calculate correlation
neighborhood_correlation <- merged_incident_data %>%
  group_by(Neighborhood) %>%
  filter(sum(!is.na(incident_count) & !is.na(crime_count)) > 1) %>%  # Ensure there are at least 2 valid pairs
  summarize(
    correlation = cor(incident_count, crime_count, use = "complete.obs"),
    total_incident_count = sum(incident_count, na.rm = TRUE),
    total_crime_count = sum(crime_count, na.rm = TRUE)
  ) %>%
  ungroup()

# View the result
print(neighborhood_correlation)





#--------------- another attempt ----------------

# Step 1: Calculate crime_count for each neighborhood from the crime_data
crime_count_by_neighborhood <- crime_data %>%
  group_by('Analysis.Neighborhood') %>%
  summarize(crime_count = n())

# Step 2: Summarize the fire_data to get incident_count for each neighborhood
incident_count_by_neighborhood <- fire_data_all %>%
  group_by('Analysis.Neighborhood') %>%
  summarize(incident_count = n())

# Step 3: Merge the two datasets on Neighborhood
merged_data <- incident_count_by_neighborhood %>%
  inner_join(crime_data_sf, by = "Analysis.Neighborhood")

# Step 4: Calculate the correlation between incident_count and crime_count
merged_data <- merged_data %>%
  mutate(correlation = cor(incident_count, crime_count))

# Step 5: Select the top 10 neighborhoods by incident_count
top_10_neighborhoods <- merged_data %>%
  arrange(desc(incident_count)) %>%
  top_n(10, wt = incident_count)

# Step 6: Create the correlation plot
ggplot(top_10_neighborhoods, aes(x = reorder(Neighborhood, correlation), y = correlation, color = correlation)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Incident Count and Their Correlation with Crime Count", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))





#--------------------------------------dots------------Top 10 Neighborhoods---- fire + ems & crime--------------------------------
# Ensure the correlation data frame exists and is sorted by correlation
ems_region_correlation <- ems_region_correlation[order(ems_region_correlation$correlation, decreasing = TRUE),]

ggplot(ems_region_correlation[1:10, ], aes(x = reorder(Neighborhood, correlation), y = correlation, color = correlation)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire + EMS and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),  # Increased title size for better visibility
        axis.title = element_text(size = 12),               # Ensure axis titles are legible
        axis.text = element_text(size = 10))                # Ensure axis labels are legible

#--------------correlation ------------

# Assuming the dataset is loaded and named `merged_data`
# and you want to calculate the correlation between `fire_count` and `crime_count`.

correlation_fire_crime <- cor(merged_data$fire_count, merged_data$crime_count, use = "complete.obs")

# Print the correlation
print(correlation_fire_crime)



#--------------------------------------dots------------Top 10 Neighborhoods---- fire & crime----------------------------------------------

ggplot(ems_region_correlation, aes(x = reorder(Neighborhood, correlation), y = correlation, color = correlation)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire + EMS and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))  


# Filter datasets to include only the specified neighborhoods
neighborhoods <- c("Tenderloin", "Mission", "South of Market", "Financial District/South Beach", "Bayview Hunters Point")

# Use the correct column names for filtering
fire_data <- fire_data %>% filter(`Neighborhooods...Analysis.Boundaries` %in% neighborhoods)
crime_data <- crime_data %>% filter(`Analysis.Neighborhood` %in% neighborhoods)

#fire_data <- fire_data %>% filter(neighborhood %in% neighborhoods)
#crime_data <- crime_data %>% filter(neighborhood %in% neighborhoods)

#---------------Spatial & Regression Analysis---------------------------

# Extract coordinates from case_location column in fire_data
fire_data <- fire_data %>%
  mutate(
    Longitude = as.numeric(str_extract(case_location, "(?<=\\().*?(?=\\s)")),
    Latitude = as.numeric(str_extract(case_location, "(?<=\\s).*?(?=\\))"))
  )

# Check if coordinates are extracted correctly
print(head(fire_data))

# Extract coordinates from case_location column in fire_data
fire_data <- fire_data %>%
  mutate(
    Longitude = as.numeric(str_extract(case_location, "(?<=\\().*?(?=\\s)")),
    Latitude = as.numeric(str_extract(case_location, "(?<=\\s).*?(?=\\))"))
  )

# Filter out rows with missing coordinates
#fire_data <- fire_data %>%
#  filter(!is.na(Longitude) & !is.na(Latitude)) 
# => this code deletes all the observations in the fire_data

#-------------above this line, problems occur & all observations are deleted-------------------
#-------------everything below here is not executed--------------------------------------------

crime_data <- crime_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))

# Ensure there are data points remaining after filtering
if (nrow(fire_data) == 0 | nrow(crime_data) == 0) {
  stop("No valid data points after filtering. Please check the data.")
}

# Convert fire data to a spatial object
fire_data_sf <- st_as_sf(fire_data, coords = c("lat", "lon"), crs = 4326)

# Convert crime data to a spatial object
crime_data_sf <- st_as_sf(crime_data, coords = c("Longitude", "Latitude"), crs = 4326)

# Check the spatial data
plot(st_geometry(fire_data_sf), col = 'blue')
plot(st_geometry(crime_data_sf), add = TRUE, col = 'red')

# Create a spatial grid
combined_bbox <- st_bbox(st_union(st_geometry(fire_data_sf), st_geometry(crime_data_sf)))
grid <- st_make_grid(st_as_sfc(combined_bbox), n = c(50, 50))

# Aggregate fire incidents
fire_aggregated <- st_join(st_as_sf(grid), fire_data_sf, join = st_intersects) %>% 
  group_by(geometry) %>% 
  summarize(fire_count = n())

# Aggregate crime incidents
crime_aggregated <- st_join(st_as_sf(grid), crime_data_sf, join = st_intersects) %>% 
  group_by(geometry) %>% 
  summarize(crime_count = n())

# Merge the aggregated data
merged_data <- left_join(st_drop_geometry(fire_aggregated), st_drop_geometry(crime_aggregated), by = "geometry")

# Convert merged data back to a spatial object
merged_data_sf <- st_as_sf(merged_data, coords = c("geometry"), crs = 4326)

# Check the merged spatial data
plot(st_geometry(merged_data_sf))

# Perform a simple linear regression
model <- lm(crime_count ~ fire_count, data = merged_data)

# Summary of the model
summary(model)

# Plot the regression
plot(merged_data$fire_count, merged_data$crime_count, main = "Fire Incidents vs Crime Incidents",
     xlab = "Fire Incidents", ylab = "Crime Incidents")
abline(model, col = "red")

# Create a spatial weights matrix
coords <- st_coordinates(merged_data_sf)
nb <- knn2nb(knearneigh(coords, k = 4))
lw <- nb2listw(nb)

# Perform a spatial lag regression
lag_model <- lagsarlm(crime_count ~ fire_count, data = merged_data, listw = lw)

# Summary of the spatial lag model
summary(lag_model)

#---------------- adding population as a control variable -------------------------------------

# Load the population data
population_data <- read.csv("/Users/chiaravonwatzdorf/Desktop/Fire Data/San_Francisco_Population_and_Demographic_Census_data_20240824.csv", header = TRUE)

# Print all values in the geography_name column
print(population_data$geography_name)

# Print all column names
print(colnames(population_data))

# Rename columns in population_data if necessary
population_data <- population_data %>%
  rename(Neighborhood = 'geography_name', # Replace with actual column name
         Population = 'estimate')    # Replace with actual column name

# Assuming the population data has columns 'geography_name', 'estimate', and 'date_column_name'
population_data <- population_data %>%
  rename(Neighborhood = 'geography_name',  # Replace with the correct column name for neighborhood
         Population = 'estimate',          # Replace with the correct column name for population estimate
         Date = 'end_year')        # Replace with the correct column name for date


# Merge the population data with the merged incident and crime data
merged_data_with_population <- merge(merged_data, population_data, by = "Neighborhood", all.x = TRUE)

# Display the first few rows of the merged data
head(merged_data_with_population)

# Perform a linear regression with population as a control variable
model_with_population <- lm(fire_count ~ crime_count + Population, data = merged_data_with_population)

# Summary of the model
summary(model_with_population)




#-------------------------filtered by 5 neighborhoods-------------------------------------------

# Convert fire data to a spatial object
fire_data_sf <- st_as_sf(fire_data, coords = c("lat", "lon"), crs = 4326)

# Convert crime data to a spatial object
crime_data_sf <- st_as_sf(crime_data, coords = c("Latitude", "Longitude"), crs = 4326)

# Check the spatial data
plot(st_geometry(fire_data_sf), col = 'blue')
plot(st_geometry(crime_data_sf), add = TRUE, col = 'red')

# Create a spatial grid
combined_bbox <- st_bbox(st_union(st_geometry(fire_data_sf), st_geometry(crime_data_sf)))
grid <- st_make_grid(st_as_sfc(combined_bbox), n = c(50, 50))

# Aggregate fire incidents
fire_aggregated <- st_join(st_as_sf(grid), fire_data_sf, join = st_intersects) %>% 
  group_by(geometry) %>% 
  summarize(fire_count = n())

# Aggregate crime incidents
crime_aggregated <- st_join(st_as_sf(grid), crime_data_sf, join = st_intersects) %>% 
  group_by(geometry) %>% 
  summarize(crime_count = n())

# Merge the aggregated data
merged_data <- left_join(st_drop_geometry(fire_aggregated), st_drop_geometry(crime_aggregated), by = "geometry")

# Convert merged data back to a spatial object
merged_data_sf <- st_as_sf(merged_data, coords = c("geometry"), crs = 4326)

# Check the merged spatial data
plot(st_geometry(merged_data_sf))

# Perform a simple linear regression
model <- lm(crime_count ~ fire_count, data = merged_data)

# Summary of the model
summary(model)

# Plot the regression
plot(merged_data$fire_count, merged_data$crime_count, main = "Fire Incidents vs Crime Incidents",
     xlab = "Fire Incidents", ylab = "Crime Incidents")
abline(model, col = "red")

# Create a spatial weights matrix
coords <- st_coordinates(merged_data_sf)
nb <- knn2nb(knearneigh(coords, k = 4))
lw <- nb2listw(nb)

# Perform a spatial lag regression
lag_model <- lagsarlm(crime_count ~ fire_count, data = merged_data, listw = lw)

# Summary of the spatial lag model
summary(lag_model)

#---------------------Explore Crime Data---------------------------------------

# Check the column names to find the correct column for crime type
colnames(crime_data)

# Assuming the crime type column is named "Incident Category"
# Replace "Incident Category" with the actual column name if it's different

# Summarize the data by neighborhood and crime type
crime_summary <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Category`) %>%
  summarize(crime_count = n(), .groups = 'drop')

# Create the bar graph
ggplot(crime_summary, aes(x = `Analysis.Neighborhood`, y = crime_count, fill = `Incident.Category`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Crimes by Neighborhood and Type",
       x = "Neighborhood",
       y = "Number of Crimes",
       fill = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------Second try------------------------------

# Create the bar graph
ggplot(crime_data, aes(x = `Analysis.Neighborhood`, y=crime_count, fill = `Incident.Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Crimes by Neighborhood and Type",
       x = "Neighborhood",
       y = "Number of Crimes",
       fill = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Use a color palette similar to the example


#----------------Third Graph--------------------------------------------------------------

# Summarize the data by neighborhood and crime type
crime_summary_2 <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Category`) %>%
  summarize(crime_count = n(), .groups = 'drop')

# Create the bar graph
ggplot(crime_summary_2, aes(x = `Analysis.Neighborhood`, y = crime_count, fill = `Incident.Category`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Number of Crimes by Neighborhood and Type",
       x = "Neighborhood",
       y = "Number of Crimes",
       fill = "Crime Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")  # Use a color palette similar to the example