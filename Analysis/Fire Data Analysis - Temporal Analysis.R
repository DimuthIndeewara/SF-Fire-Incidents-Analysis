#install.packages("sf")
if (!require("lubridate")) install.packages("lubridate")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("scales")) install.packages("scales")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("sf")) install.packages("sf")
if (!require("ggmap")) install.packages("ggmap")
if (!require("viridis")) install.packages("viridis")
if (!require("ggridges")) install.packages("ggridges")



library(sf)
library(ggmap)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(gridExtra)
library(scales)
library(RColorBrewer)
library(corrplot)
library(viridis)
library(ggridges)
library(patchwork)


# -------------------------------------------------------------------------------------------------------------------------
#rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ====================================================  Data ==============================================================

data_all <- read.csv("filtered_data_2018_2024.csv", header = TRUE)

data_climate <- read.csv("Climate Data 2018 - 2024.csv", header = TRUE) # https://www.ncei.noaa.gov/data/local-climatological-data/doc/LCD_documentation.pdf

# ================================================= Data Preprocessing =====================================================

data_climate$DATE <- as.POSIXct(data_climate$DATE, format = "%Y-%m-%d")


data_climate_select <-  data_climate %>% select(c("DATE","DailyMaximumDryBulbTemperature",
                                                  "DailyMinimumDryBulbTemperature","DailyAverageDryBulbTemperature")) %>%
  filter(!is.na(DailyAverageDryBulbTemperature))

data_climate_select <- data_climate_select %>%
  rename(
    Date = DATE,
    Max_Temperature = DailyMaximumDryBulbTemperature,
    Min_Temperature = DailyMinimumDryBulbTemperature,
    Avg_Temperature = DailyAverageDryBulbTemperature
  )


data <- data_all %>% select(-"new_call_date")


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


date_columns <- c("Call.Date", "Watch.Date") 

# Apply as.Date conversion to each column in date_columns
data[date_columns] <- lapply(data[date_columns], as.Date, format = "%m/%d/%Y")

data <- data %>%
  filter(!(year(Call.Date) == 2024 & month(Call.Date) == 6)) #Remove 2024 Jun,Since the dataset is incomplete in the month of 2024 Jun

data <- data %>%
  left_join(data_climate_select, by = c("Call.Date" = "Date"))


# Check Column wise NA in the dataset
colSums(is.na(data)) 

# Check blank data in the dataset
blank_counts <- sapply(data, function(x) {
  if (is.character(x)) {
    sum(x == "", na.rm = TRUE)
  } else {
    0
  }
})

blank_counts

# Identify columns with any blank values
blank_columns <- names(blank_counts[blank_counts > 0])
blank_columns

# Remove observation if any blank values in the case_location & Call.Type.Group
data <- data %>% filter(case_location != "" & Call.Type.Group != "")

str(data)
colnames(data)
summary(data)


--------------------------------------------------------------------------------------------------------------------------------------------------

datetime_columns <- c("Received.DtTm", "Response.DtTm", "Hospital.DtTm", "Entry.DtTm", "On.Scene.DtTm", "Dispatch.DtTm",
                      "Transport.DtTm","Available.DtTm","data_as_of", "data_loaded_at" )

# Apply as.POSIXct conversion to each column in datetime_columns
data[datetime_columns] <- lapply(data[datetime_columns], as.POSIXct, format = "%m/%d/%Y %I:%M:%S %p")


# Extract Incident Year,Month & Hour
data$Year = year(data$Call.Date)
data$Month = month(data$Call.Date,label = TRUE)
data$Day = day(data$Dispatch.DtTm)
data$Hour = hour(data$Dispatch.DtTm)


data$Season <- ifelse(month(data$Call.Date) %in% c(6, 7, 8), "Summer",
                      ifelse(month(data$Call.Date) %in% c(12, 1, 2), "Winter", 
                             ifelse(month(data$Call.Date) %in% c(3, 4, 5), "Spring", "Autumn")))

# Summarize data into Year, Month and Season
df_monthly <- data %>%
  group_by(Year, Month,Season) %>%
  summarise(count = n()) %>%
  ungroup()



# Year has been adjusted based on the season, When in the Winter Jan & Feb should belong to the  previous year.   
data <- data %>%
  mutate(Adjusted_Year = ifelse(Month %in% c("Jan", "Feb"), Year - 1, Year))

# Summarize data into Year, Hour and Season
df_hour <- data %>%
  group_by(Adjusted_Year,Hour,Season) %>%
  summarise(count = n()) %>%
  ungroup()

plots <- list()

# Loop through each season and create plots
for (season in unique(df_hour$Season)) {
  plot <- ggplot(df_hour %>% filter(Season == season), aes(x = Hour, y = count, color = as.factor(Adjusted_Year), group = Adjusted_Year)) +
    geom_line(size = 0.75) +
    labs(title = paste("Fire incidents in", season),
         x = "Hour",
         y = "No. of Incident",
         color = "Year") +
    theme_classic()
  
  plots[[season]] <- plot
}

grid.arrange(
  plots[["Summer"]],
  plots[["Winter"]],
  plots[["Spring"]],
  plots[["Autumn"]],
  ncol = 1  # Number of rows in the grid (adjust as needed)
)

# Summarize data into Year, Month and Season
df_ad_year <- data %>%
  group_by(Adjusted_Year, Day,Season) %>%
  summarise(count = n()) %>%
  ungroup()

for (season in unique(df_ad_year$Season)) {
  plot <- ggplot(df_ad_year %>% filter(Season == season), aes(x = Day, y = count, color = as.factor(Adjusted_Year), group = Adjusted_Year)) +
    geom_line(size = 0.75) +
    labs(title = paste("Fire incidents in", season),
         x = "Day of the Month",
         y = "No. of Incident",
         color = "Year") +
    theme_classic()

  plots[[season]] <- plot
}

grid.arrange(
  plots[["Summer"]],
  plots[["Winter"]],
  ncol = 1  
)

# ------------------------ Fire Incident across Seasons ------------------------

# Summarize data into Year, Month and Season
df_season <- data %>%
  group_by(Season,Adjusted_Year) %>%
  summarise(count = n()) %>%
  ungroup()
df_season <- df_season %>% filter(Adjusted_Year >= 2018 & Adjusted_Year <= 2024)


season_colors <- c(
  "Winter" = "#1f77b4",   # Blue
  "Autumn" = "#ff7f0e",  # Orange
  "Spring" = "#2ca02c",  # Green
  "Summer" = "#d62728"  # Red
)

df_season$Season <- factor(df_season$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot all seasons in one plot with visually appealing colors
plot_7 <-ggplot(df_season, aes(x = Adjusted_Year, y = count, color = Season)) +
          geom_line(size = 0.75) +
          labs(title = "Fire Incidents across Seasons (2018 - 2023 )",
               x = "Year",
               y = "Number of Incidents",
               color = "Season") +
          scale_x_continuous(limits = c(2018, 2023), breaks = 2018:2024) +
          scale_y_continuous(limits = c(22000, 32000)) +
          scale_color_manual(values = season_colors) +
          theme_classic() + 
          theme(legend.position = "bottom") 

plot_7

ggsave("fireIncident_across_season.png", plot = plot_7, width = 10, height = 8, dpi = 300)


df_season$Season <- factor(df_season$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with reordered seasons
plot_8 <-ggplot(df_season, aes(x = Adjusted_Year, y = count, color = Season)) +
        geom_smooth(method = "loess", se = FALSE , size = 0.75) +
        labs(title = "Fire Incidents Changes in Season (2018 - 2023)",
             x = "Year",
             y = "Number of Incidents") +
        scale_x_continuous(limits = c(2018, 2023), breaks = 2018:2023) +
        scale_y_continuous(limits = c(22000, 32000)) +
        scale_color_manual(values = season_colors) +
        theme_classic() + 
        theme(
          legend.position = "bottom",
          plot.title = element_text(hjust = 0.5)
        ) +
        facet_wrap(~ Season, scales = "free_y")
plot_8

ggsave("fireIncident_Changes_season.png", plot = plot_8, width = 10, height = 8, dpi = 300)



# -------------------------------Top Fire categories in each season--------------------------------------------
df_category_counts <- data %>%
  group_by(Season, Adjusted_Year, Call.Type) %>%
  summarise(Total_Count = n()) %>%
  ungroup()

# Find the top category for each season
top_categories_season <- df_category_counts %>%
  group_by(Season) %>%
  top_n(n = 24, wt = Total_Count) %>%
  ungroup()

top_categories_season <- top_categories_season %>%
  group_by(Season, Adjusted_Year) %>%
  mutate(Call.Type = reorder(Call.Type, -Total_Count)) %>%
  filter(!Adjusted_Year %in% c("2017")) %>% 
  ungroup()

top_categories_season$Season <- factor(top_categories_season$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with reordered seasons
ggplot(top_categories_season, aes(x = as.factor(Adjusted_Year), y = Total_Count, fill = Call.Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Top Call Type in each Season (2018 - 2023)",
       x = "Year",
       y = "Total Count of Incidents",
       fill = "Call Type") +
  facet_wrap(~Season, scales = "free") +  # Facet by Season
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_category_wa <- df_category_counts %>% 
  filter(!Call.Type %in% c( "Medical Incident","Alarms")) %>% 
  filter(!Adjusted_Year %in% c("2017"))

top_categories_wa <- df_category_wa %>%
  group_by(Season) %>%
  top_n(n = 24, wt = Total_Count) %>%
  ungroup()

top_categories_wa <- top_categories_wa %>%
  group_by(Season, Adjusted_Year) %>%
  mutate(Call.Type = reorder(Call.Type, -Total_Count)) %>%
  ungroup()

top_categories_wa$Season <- factor(top_categories_wa$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

plot_4 <-ggplot(top_categories_wa, aes(x = as.factor(Adjusted_Year), y = Total_Count, fill = Call.Type)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_brewer(palette = "RdYlGn", direction = 1) +
          labs(title = "Top Call Type in each Season (2018 - 2023)",
               x = "Year",
               y = "Number of Incidents",
               fill = "Call Type") +
          facet_wrap(~Season, scales = "free") +  # Facet by Season
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_4

ggsave("Calltype_season.png", plot = plot_4, width = 10, height = 8, dpi = 300)

# ------------------------------------------------------------------------------------------------------------------

df_category_call <- data %>%
  group_by(Season, Call.Type) %>%
  summarise(Total_Count = n()) %>%
  ungroup()

top_categories_call <- df_category_call %>%
  group_by(Season) %>%
  slice_max(order_by = Total_Count, n = 6) %>%
  ungroup()

top_categories_call <- top_categories_call %>%
  arrange(Season, desc(Total_Count)) %>%
  mutate(Call.Type = factor(Call.Type, levels = unique(Call.Type)))

top_categories_call$Season <- factor(top_categories_call$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with reordered seasons
ggplot(top_categories_call, aes(x = Call.Type, y = Total_Count, fill = Call.Type)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  labs(title = "Top Call Types in each Season (2018 - 2023)",
       y = "Total Count of Incidents") +
  facet_wrap(~Season, scales = "free") +  # Facet by Season
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -------------------------------Top Fire categories in each neighborhood--------------------------------------------
df_category_neigh <- data %>%
  group_by(Season, Adjusted_Year, Neighborhooods...Analysis.Boundaries) %>%
  summarise(Total_Count = n()) %>%
  ungroup()

# Find the top category for each season
top_categories_neigh <- df_category_neigh %>%
  group_by(Season ) %>%
  top_n(n = 28, wt = Total_Count) %>%
  ungroup()

top_categories_neigh$Season <- factor(top_categories_neigh$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with reordered seasons
plot_5 <- ggplot(top_categories_neigh, aes(x = as.factor(Adjusted_Year), y = Total_Count, fill = Neighborhooods...Analysis.Boundaries)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_brewer(palette = "RdYlGn", direction = -1) +
          labs(title = "Top Fire Regions in each Season (2018 - 2024)",
               x = "Year",
               y = "Number of Incidents",
               fill = "Neighborhood") +
          facet_wrap(~Season, scales = "free") +  # Facet by Season
          theme_classic() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_5

ggsave("region_season.png", plot = plot_5, width = 10, height = 8, dpi = 300)
# ------------------------------ Box plots for each season ----------------

top_categories_season$Season <- factor(top_categories_season$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
ggplot(top_categories_season, aes(x = Season, y = Total_Count, fill = Season)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Fire Incident Types by Season",
    x = "Season",
    y = "Total Count of Incidents"
  ) +
  scale_fill_manual(values = season_colors) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

top_categories_season_filtered <- top_categories_season %>%
  filter(Adjusted_Year >= 2018 & Adjusted_Year <= 2023)

# Create the box plot
top_categories_season_filtered$Season <- factor(top_categories_season_filtered$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
ggplot(top_categories_season_filtered, aes(x = as.factor(Adjusted_Year), y = Total_Count, fill = Season)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Fire Incident Types by Season (2018 - 2023)",
    x = "Year",
    y = "Total Count of Incidents"
  ) +
  scale_fill_manual(values = season_colors) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# ------------------------------  Time Series Analysis ----------------------------

monthly_incidents <- data %>%
  group_by(month = floor_date(Call.Date, "month")) %>%
  filter(year(month) < 2024) %>% 
  summarise(incident_count = n())

ts_incidents <- ts(monthly_incidents$incident_count, start = c(year(min(monthly_incidents$month)),
                                                               month(min(monthly_incidents$month))), frequency = 12)

decomposed_ts <- stl(ts_incidents, s.window = "periodic")


# Extract components
trend <- decomposed_ts$time.series[, "trend"]
seasonal <- decomposed_ts$time.series[, "seasonal"]
remainder <- decomposed_ts$time.series[, "remainder"]

# Plot components
par(mfrow = c(3, 1))
plot(trend, main = "Trend Component")
plot(seasonal, main = "Seasonal Component")
plot(remainder, main = "Remainder Component")



# ---------------------------------- Temperature Changes ---------------------------------------

seasonal_temps <- data %>%
  group_by(Adjusted_Year, Season) %>%
  summarise(
    Mean_Min_Temperature = min(Min_Temperature, na.rm = TRUE),
    Mean_Max_Temperature = max(Max_Temperature, na.rm = TRUE)
  )

seasonal_temps <- seasonal_temps %>%
  filter(Adjusted_Year >= 2018 & Adjusted_Year <= 2023)

seasonal_temps$Adjusted_Year <- as.factor(seasonal_temps$Adjusted_Year)

# Plotting grouped bar chart
seasonal_temps$Season <- factor(seasonal_temps$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
plot_9 <-ggplot(seasonal_temps, aes(x = Adjusted_Year, fill = Season)) +
          geom_bar(aes(y = Mean_Min_Temperature), position = "dodge", stat = "identity", color = "black", width = 0.5) +
          geom_bar(aes(y = Mean_Max_Temperature), position = "dodge", stat = "identity", color = "black", width = 0.5, alpha = 0.5) +
          labs(title = "Minimum and Maximum Temperatures in SF (2018 - 2023)",
               x = "Year",
               y = "Temperature [F]",
               fill = "Season") +
          scale_fill_manual(values = season_colors) +  # Use custom colors
          theme_classic() +
          theme(legend.position = "bottom")

plot_9

ggsave("temperature.png", plot = plot_9, width = 10, height = 8, dpi = 300)

# --------------------------- Analysisng the correlation with temperature ---------------------------------

daily_fire_data <- data %>%
  group_by(Call.Date, Season) %>%
  summarise(daily_fire_incidents = n())

daily_fire_data <- daily_fire_data %>%
  left_join(data_climate_select, by = c("Call.Date" = "Date"))


daily_fire_data <- daily_fire_data %>%
  filter(!is.na(daily_fire_incidents), !is.na(Avg_Temperature))

correlation_result <- cor(daily_fire_data$daily_fire_incidents, daily_fire_data$Avg_Temperature)
print(correlation_result)

seasonal_correlation <- daily_fire_data %>%
  group_by(Season) %>%
  summarise(correlation = cor(daily_fire_incidents, Avg_Temperature))

ggplot(daily_fire_data, aes(x = Avg_Temperature, y = daily_fire_incidents)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Fire Incidents and Average Temperature",
       x = "Average Temperature",
       y = "Daily Fire Incidents") +
  theme_minimal()

print(seasonal_correlation)

daily_fire_data$Season <- factor(daily_fire_data$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
ggplot(daily_fire_data, aes(x = Avg_Temperature, y = daily_fire_incidents, color = Season)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Fire Incidents and Average Temperature by Season",
       x = "Average Temperature",
       y = "Daily Fire Incidents") +
  scale_color_manual(values = season_colors) +  # Use custom colors
  theme_minimal() +
  facet_wrap(~ Season)


daily_fire_data <- daily_fire_data %>%
  mutate(Year = year(Call.Date),
         Month = month(Call.Date, label = TRUE))

daily_fire_data <- daily_fire_data %>%
  mutate(Adj_Year = ifelse(Month %in% c("Jan", "Feb"), Year -1, Year ))

seasonal_yearly_correlation <- daily_fire_data %>%
  group_by(Season, Adj_Year) %>%
  summarise(correlation = cor(daily_fire_incidents, Avg_Temperature, use = "complete.obs")) %>% 
  filter( !Adj_Year == 2024)

print(seasonal_yearly_correlation)

seasonal_yearly_correlation$Season <- factor(seasonal_yearly_correlation$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
ggplot(seasonal_yearly_correlation, aes(x = Adj_Year, y = correlation, color = Season)) +
  geom_line() +
  geom_point() +
  labs(title = "Correlation between Fire Incidents and Average Temperature (2018 - 2023)",
       x = "Year",
       y = "Correlation") +
  scale_color_manual(values = season_colors) +  # Use custom colors
  scale_x_continuous(limits = c(2018, 2023), breaks = 2018:2023) +
  facet_wrap(~ Season) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(size = 10))

seasonal_yearly_correlation$Season <- factor(seasonal_yearly_correlation$Season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Plot with custom colors
ggplot(seasonal_yearly_correlation, aes(x = Adj_Year, y = correlation, color = Season)) +
  geom_line(size = 1) +  # Add lines for the correlations
  geom_point(size = 3) +  # Add points for each data point
  labs(title = "Correlation between Fire Incidents and Average Temperature (2018 - 2023)",
       x = "Year",
       y = "Correlation") +
  scale_color_manual(values = season_colors) +  # Use custom colors for lines and points
  scale_x_continuous(limits = c(2018, 2023), breaks = 2018:2023) +
  theme_classic() +
  theme(
    legend.position = "bottom",  # Move the legend to the bottom
    plot.title = element_text(size =12)
  )

seasonal_yearly_correlation <- seasonal_yearly_correlation %>%
  filter(Adj_Year != 2017)

plot_10 <-ggplot(seasonal_yearly_correlation, aes(x = Adj_Year, y = Season, fill = correlation)) +
          geom_tile() +
          scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", 
                               midpoint = 0, name = "Correlation") +
          labs(title = "Heatmap of Correlation between Fire Incidents and Average Temperature",
               x = "Year",
               y = "Season") +
          theme_classic() +
          theme(
            plot.title = element_text(hjust = 0.5, size = 12)  # Center the plot title
          )

plot_10

ggsave("temp_corr.png", plot = plot_10, width = 10, height = 8, dpi = 300)


# ----------------------------------------- Linear Regression ----------------------------------------------------
# Perform a simple linear regression
model <- lm(daily_fire_incidents ~ Avg_Temperature, data = daily_fire_data)

# Print the summary of the model
summary(model)


# ----------------------------------------- Response Time Analysis ------------------------------------------------


colnames(data)

data <- data %>%
  mutate(Dispatch_Time = as.numeric(difftime(Dispatch.DtTm, Received.DtTm, units = "mins")),
         Response_Time = as.numeric(difftime(Response.DtTm, Dispatch.DtTm, units = "mins")),
         Travel_Time = as.numeric(difftime(On.Scene.DtTm, Response.DtTm, units = "mins")),
         Travel_Time_to_Hospital = as.numeric(difftime(Hospital.DtTm, Transport.DtTm, units = "mins")),
         Total_Response_Time = as.numeric(difftime(On.Scene.DtTm, Received.DtTm, units = "mins")),
         Total_occupied_Time = as.numeric(difftime(Available.DtTm, Received.DtTm, units = "mins")))


summary(data[c("Dispatch_Time", "Response_Time", "Travel_Time", "Travel_Time_to_Hospital","Total_Response_Time","Total_occupied_Time")])

fire_data_long <- data %>%
  select(Dispatch_Time, Response_Time, Travel_Time,
         Travel_Time_to_Hospital, Total_Response_Time, Total_occupied_Time) %>%
  pivot_longer(everything(), names_to = "Response_Type", values_to = "Time")





