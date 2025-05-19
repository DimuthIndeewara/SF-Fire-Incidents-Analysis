# Load necessary libraries
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("scales")) install.packages("scales")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("sf")) install.packages("sf")
if (!require("ggmap")) install.packages("ggmap")
if (!require("viridis")) install.packages("viridis")


library(sf)
library(ggmap)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(viridis)
library(broom)
library(reshape2)


# Load necessary libraries
library(tidyverse)
library(sf) # for spatial data
library(spdep) # for spatial regression
library(spatialreg) # for spatial regression

# Set the working directory
#setwd("F:/Master TU Dresden/SoSe 2024/Application in Data Analytics")

rm(list = ls())

setwd("/Users/chiaravonwatzdorf/Desktop/Fire Data")

# Read the data
data <- read.csv("filtered_data_2018_2024.csv", header = TRUE)

#/Users/chiaravonwatzdorf/Desktop/Fire Data/filtered_data_2018_2024.csv

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

# Fire Dataset
fire_data <- data %>%
  filter(Category %in% "Fire")

head(fire_data)

# EMS Dataset
EMS_data <- data %>%
  filter(Category %in% "EMS")


crime_data <- read.csv("Police_Department_Incident_Reports__2018_to_Present_20240709.csv", header = TRUE)

# Filter data sets to include only the specified neighborhoods
# neighborhoods <- c("Tenderloin", "Mission", "South of Market", "Financial District/South Beach", "Bayview Hunters Point")
# 
# # Filter fire data
# fire_data <- fire_data %>% filter(`Neighborhooods...Analysis.Boundaries` %in% neighborhoods)

str(fire_data)

# Filter crime data
#crime_data <- crime_data %>% filter(`Analysis.Neighborhood` %in% neighborhoods)

#-------------this produces an error message-------------------------------------------------------
# Convert date columns to Date type
fire_data <- fire_data %>% mutate(`Call.Date` = as.Date(`Call.Date`, format="%m/%d/%Y"))
EMS_data <- EMS_data %>% mutate(`Call.Date` = as.Date(`Call.Date`, format="%m/%d/%Y"))
crime_data <- crime_data %>% mutate(`Incident.Date` = as.Date(`Incident.Datetime`, format="%Y/%m/%d"))
#---------------------------------------------------------------------------------------------------

# Aggregate fire incidents by neighborhood and month
fire_data_sf <- fire_data %>%
  #mutate(month = floor_date(`Call.Date`)) %>%
  group_by(`Neighborhooods...Analysis.Boundaries`,`Call.Date`) %>%
  summarize(fire_count = n(), .groups = 'drop')

# Aggregate crime incidents by neighborhood and month

crime_data_sf <- crime_data %>%
  group_by(`Analysis.Neighborhood`,`Incident.Date`) %>%
  summarize(crime_count = n(), .groups = 'drop') %>% 
  filter(!is.na(`Analysis.Neighborhood`) & `Analysis.Neighborhood` != "") 
 

# Merge the aggregated data
merged_data <- merge(fire_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries","Call.Date"), by.y = c("Analysis.Neighborhood","Incident.Date"))

colnames(merged_data)

crime_region_correlation <- merged_data %>%
  group_by(Neighborhooods...Analysis.Boundaries) %>%
  summarise(correlation = cor(fire_count, crime_count, use = "complete.obs")) %>% 
  arrange(desc(correlation)) %>%  
  slice_head(n = 10)
  

print(crime_region_correlation)

#-----------Top 10 Neighborhoods------bars-------------------
ggplot(crime_region_correlation, aes(x = reorder(Neighborhooods...Analysis.Boundaries, correlation), y = correlation, fill = correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make labels easier to read
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire and Crime Counts",
       x = "Neighborhood",
       y = "Correlation") +
  theme_classic()

#--------------------------------Top 10 Neighborhoods------dots/points-------------------

ggplot(crime_region_correlation, aes(x = reorder(Neighborhooods...Analysis.Boundaries, correlation), y = correlation, color = correlation)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Tope 10 Neighborhoods with Highest Correlation between Fire and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))  


# ------------------------------ Medical data vs Fire Incident ----------------------------


ems_data_sf <- EMS_data %>%
  group_by(`Neighborhooods...Analysis.Boundaries`,`Call.Date`) %>%
  summarize(ems_count = n(), .groups = 'drop')
# Merge the aggregated data
merged_data_ems <- merge(ems_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries","Call.Date"), by.y = c("Analysis.Neighborhood","Incident.Date"))

colnames(merged_data_ems)

crime_ems_region_correlation <- merged_data_ems %>%
  group_by(Neighborhooods...Analysis.Boundaries) %>%
  summarise(correlation = cor(ems_count, crime_count, use = "complete.obs")) %>% 
  arrange(desc(correlation)) %>%  
  slice_head(n = 10)


print(crime_ems_region_correlation)

ggplot(crime_ems_region_correlation, aes(x = reorder(Neighborhooods...Analysis.Boundaries, correlation), y = correlation, fill = correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates to make labels easier to read
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire and Crime Counts",
       x = "Neighborhood",
       y = "Correlation") +
  theme_classic()

ggplot(crime_ems_region_correlation, aes(x = reorder(Neighborhooods...Analysis.Boundaries, correlation), y = correlation, color = correlation)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Tope 10 Neighborhoods with Highest Correlation between Fire and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(plot.title = element_text(size = 10, hjust = 0.5))  

crime_region_correlation$type <- "Crime"
crime_ems_region_correlation$type <- "EMS"
combined_data <- bind_rows(crime_region_correlation, crime_ems_region_correlation)

# Plotting
ggplot(combined_data, aes(x = reorder(Neighborhooods...Analysis.Boundaries, correlation), y = correlation, color = correlation, shape = type)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 0, name = "Correlation") +
  scale_shape_manual(values = c(16, 17), name = "Type") + # Different shapes for Crime and EMS
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )




# Perform a simple linear regression
model <- lm(crime_count ~ fire_count, data = merged_data)

# Print the summary of the model
summary(model)

#scatterplot
ggplot(merged_data, aes(x=fire_count, y=crime_count)) + geom_point() + theme_minimal()

#--- until here everything fine
#t-test
t.test(merged_data$fire_count ~ merged_data$Neighborhooods...Analysis.Boundaries) 

# ANOVA 
anova_result <- aov(fire_count ~ Neighborhooods...Analysis.Boundaries, data=merged_data) 

summary(anova_result)

#boxplot
ggplot(merged_data, aes(x = fire_count, y = crime_count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Crime Counts vs Fire Counts",
       x = "Number of Fire Incidents",
       y = "Number of Crime Incidents") +
  theme_minimal()

#-----------------two boxplots-------------------

# Create boxplot for crime_data_sf
crime_boxplot <- ggplot(crime_data_sf, aes(y = crime_count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Crime Counts",
       y = "Number of Crime Incidents") +
  theme_minimal()

# Create boxplot for fire_data_sf
fire_boxplot <- ggplot(fire_data_sf, aes(y = fire_count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Fire Counts",
       y = "Number of Fire Incidents") +
  theme_minimal()

# Display the boxplots side by side
grid.arrange(crime_boxplot, fire_boxplot, ncol = 2)

#---------------------

# Plot the correlation and regression line
ggplot(merged_data, aes(x = fire_count, y = crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Correlation between Fire Incidents and Crime Incidents",
       x = "Number of Fire Incidents",
       y = "Number of Crime Incidents") +
  theme_minimal()

#-----------------------Crime Type Correlation with Fire Incidents--------------

# Aggregate fire incidents by neighborhood and date
fire_data_sf <- fire_data %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Call.Date`) %>%
  summarize(fire_count = n(), .groups = 'drop')

# Aggregate crime incidents by neighborhood, date, and crime type
crime_data_sf <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Date`, `Incident.Category`) %>%
  summarize(crime_count = n(), .groups = 'drop') %>%
  filter(!is.na(`Analysis.Neighborhood`) & `Analysis.Neighborhood` != "") 

# Merge the fire and crime data by neighborhood and date
merged_data_fire <- merge(fire_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries", "Call.Date"), by.y = c("Analysis.Neighborhood", "Incident.Date"))

# Calculate the correlation for each crime type with fire counts
crime_fire_correlation <- merged_data_fire %>%
  group_by(`Incident.Category`) %>%
  summarise(correlation = cor(fire_count, crime_count, use = "complete.obs")) %>%
  arrange(desc(correlation))

print(crime_fire_correlation)

# Plotting the correlations
ggplot(crime_fire_correlation, aes(x = reorder(`Incident.Category`, correlation), y = correlation, fill = correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Correlation between Fire Incidents and Different Types of Crime",
       x = "Type of Crime",
       y = "Correlation") +
  theme_classic()

# Display a scatter plot for the crime type with the highest correlation
top_crime_type <- crime_fire_correlation %>%
  slice_max(correlation, n = 1) %>%
  pull(`Incident.Category`)

top_crime_data <- merged_data_fire %>%
  filter(`Incident.Category` == top_crime_type)

ggplot(top_crime_data, aes(x = fire_count, y = crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = paste("Correlation between Fire Incidents and", top_crime_type, "Incidents"),
       x = "Number of Fire Incidents",
       y = paste("Number of", top_crime_type, "Incidents")) +
  theme_minimal()

#------------better scatterplots------------------------------------

# Aggregate EMS incidents by neighborhood and date
ems_data_sf <- EMS_data %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Call.Date`) %>%
  summarize(ems_count = n(), .groups = 'drop')

# Aggregate fire incidents by neighborhood and date
fire_data_sf <- fire_data %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Call.Date`) %>%
  summarize(fire_count = n(), .groups = 'drop')

# Aggregate crime incidents by neighborhood, date, and crime type
crime_data_sf <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Date`, `Incident.Category`) %>%
  summarize(crime_count = n(), .groups = 'drop') %>%
  filter(!is.na(`Analysis.Neighborhood`) & `Analysis.Neighborhood` != "") 

# Merge the EMS and crime data by neighborhood and date
merged_data_ems <- merge(ems_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries", "Call.Date"), by.y = c("Analysis.Neighborhood", "Incident.Date"))

# Merge the fire and crime data by neighborhood and date
merged_data_fire <- merge(fire_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries", "Call.Date"), by.y = c("Analysis.Neighborhood", "Incident.Date"))

# Calculate the correlation for each crime type with EMS counts
crime_ems_correlation <- merged_data_ems %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Incident.Category`) %>%
  summarise(correlation = cor(ems_count, crime_count, use = "complete.obs")) %>%
  arrange(desc(correlation))

# Calculate the correlation for each crime type with fire counts
crime_fire_correlation <- merged_data_fire %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Incident.Category`) %>%
  summarise(correlation = cor(fire_count, crime_count, use = "complete.obs")) %>%
  arrange(desc(correlation))

# Select top 10 neighborhoods with highest correlations for both EMS and fire
top_ems_correlation <- crime_ems_correlation %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 10)

top_fire_correlation <- crime_fire_correlation %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 10)

# Combine the top correlations for EMS and fire
combined_data <- bind_rows(
  top_ems_correlation %>% mutate(type = "EMS"),
  top_fire_correlation %>% mutate(type = "Fire")
)

# Plotting the combined correlations
ggplot(combined_data, aes(x = reorder(`Neighborhooods...Analysis.Boundaries`, correlation), y = correlation, color = correlation, shape = type)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 0.2, name = "Correlation") +
  scale_shape_manual(values = c(16, 17), name = "Type") + # Different shapes for Crime and EMS
  labs(title = "Top 10 Neighborhoods with Highest Correlation between Fire/EMS and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

#--------------only EMS plots---------

# Calculate the correlation for each crime type with EMS counts
crime_ems_correlation <- merged_data_ems %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Incident.Category`) %>%
  summarise(correlation = cor(ems_count, crime_count, use = "complete.obs")) %>%
  arrange(desc(correlation))

# Select top 10 neighborhoods with highest correlations for EMS
top_ems_correlation <- crime_ems_correlation %>%
  arrange(desc(correlation)) %>%
  slice_head(n = 10)


# Plotting the EMS correlations
ggplot(top_ems_correlation, aes(x = reorder(`Neighborhooods...Analysis.Boundaries`, correlation), y = correlation, color = correlation, shape = `Incident.Category`)) +
  geom_point(size = 4) +
  scale_color_gradient2(low = "yellow", mid = "orange", high = "red", midpoint = 0.2, name = "Correlation") +
  labs(title = "Top 10 Neighborhoods with Highest Correlation between EMS and Crime Counts", 
       x = "Neighborhood",
       y = "Correlation") +
  coord_flip() +
  theme_classic() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), 
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom"
  )

#-----------------------Crime Type Correlation with EMS----------------------------

# Aggregate EMS incidents by neighborhood and date
ems_data_sf <- EMS_data %>%
  group_by(`Neighborhooods...Analysis.Boundaries`, `Call.Date`) %>%
  summarize(ems_count = n(), .groups = 'drop')

# Aggregate crime incidents by neighborhood, date, and crime type
crime_data_sf <- crime_data %>%
  group_by(`Analysis.Neighborhood`, `Incident.Date`, `Incident.Category`) %>%
  summarize(crime_count = n(), .groups = 'drop') %>%
  filter(!is.na(`Analysis.Neighborhood`) & `Analysis.Neighborhood` != "") 

# Merge the EMS and crime data by neighborhood and date
merged_data_ems <- merge(ems_data_sf, crime_data_sf, by.x = c("Neighborhooods...Analysis.Boundaries", "Call.Date"), by.y = c("Analysis.Neighborhood", "Incident.Date"))

# Calculate the correlation for each crime type with EMS counts
crime_ems_correlation <- merged_data_ems %>%
  group_by(`Incident.Category`) %>%
  summarise(correlation = cor(ems_count, crime_count, use = "complete.obs")) %>%
  arrange(desc(correlation))

print(crime_ems_correlation)

# Plotting the correlations
ggplot(crime_ems_correlation, aes(x = reorder(`Incident.Category`, correlation), y = correlation, fill = correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Correlation") +
  labs(title = "Correlation between EMS Incidents and Different Types of Crime",
       x = "Type of Crime",
       y = "Correlation") +
  theme_classic()

# Display a scatter plot for the crime type with the highest correlation
top_crime_type <- crime_ems_correlation %>%
  slice_max(correlation, n = 1) %>%
  pull(`Incident.Category`)

top_crime_data <- merged_data_ems %>%
  filter(`Incident.Category` == top_crime_type)

ggplot(top_crime_data, aes(x = ems_count, y = crime_count)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = paste("Correlation between EMS Incidents and", top_crime_type, "Incidents"),
       x = "Number of EMS Incidents",
       y = paste("Number of", top_crime_type, "Incidents")) +
  theme_minimal()


