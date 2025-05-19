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


#======================================================= EDA =========================================================

category_counts <- data %>%
  group_by(Category) %>%
  summarise(Count = n()) %>%
  mutate(Proportion = Count / sum(Count)) 

# Proportion of each Fire Categories
ggplot(category_counts, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Proportion of each Fire Categories") +
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(Proportion)), 
            position = position_stack(vjust = 0.5), color = "black", size = 4)

# Fire Cat
plot_1 <- ggplot(category_counts, aes(x = 2, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  scale_fill_brewer(palette = "YlOrBr", direction = -1) +
  coord_polar("y", start = 0) +
  theme_void() +
  labs(title = "Proportion of each Fire Category") +
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(Proportion)), 
            position = position_stack(vjust = 0.5), color = "black", size = 4) +
  xlim(0.5, 2.5)


# Save the plot
ggsave("fire_category_donut_chart.png", plot_1, width = 8, height = 6)


data <- data %>%
   filter(!Category %in% c("EMS","Other"))


data_summarized <- data %>%
  group_by(Call.Final.Disposition) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

top_10_data <- data_summarized %>%
  slice(1:10)

 
# Summarize Call.Type counts
df_summarize_call <- data %>%
  count(Call.Type, sort = TRUE) %>%
  slice(1:10) %>%
  pull(Call.Type)

# Summarize top Neighborhoods
df_summarize_neighborhood <- data %>%
  count(Neighborhooods...Analysis.Boundaries, sort = TRUE) %>%
  slice(1:15) %>%
  pull(Neighborhooods...Analysis.Boundaries)
# 
# # Filter data to include top Call.Types and top Neighborhoods
df_region_filtered <- data %>%
  filter(Call.Type %in% df_summarize_call & Neighborhooods...Analysis.Boundaries %in% df_summarize_neighborhood) %>%
  group_by(Neighborhooods...Analysis.Boundaries, Call.Type) %>%
  summarise(count = n()) %>%
  arrange(Neighborhooods...Analysis.Boundaries, Call.Type, desc(count))

# # Plotting with ggplot
ggplot(df_region_filtered, aes(fill = Call.Type, x = reorder(Neighborhooods...Analysis.Boundaries, -count), y = count)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(
    title = "Fire Incident Types by Neighborhood",
    x = "Neighborhood",
    y = "Number of Fire Incidents"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 60, hjust = 1)
  )



call_counts <- data %>%
  group_by(Call.Type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>% 
  slice_head(n = 8) %>% 
  mutate(Proportion = round(Count / sum(Count),2)) %>%
  arrange(desc(Proportion))  

call_counts$Call.Type <- factor(call_counts$Call.Type, levels = call_counts$Call.Type[order(call_counts$Proportion, decreasing = TRUE)])

# Call Type Pie Chart
plot_3 <- ggplot(call_counts, aes(x = "", y = Proportion, fill = Call.Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  
  scale_fill_brewer(palette = "YlOrRd", direction = -1) +  
  theme_void() +  
  theme(legend.position = "right") +
  geom_text(aes(label = scales::percent(Proportion)), 
            position = position_stack(vjust = 0.5), color = "black", size = 4)



# Save the pie chart to a file
ggsave("Calltype_pie_chart.png", plot = plot_3, width = 10, height = 8, dpi = 300)


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





