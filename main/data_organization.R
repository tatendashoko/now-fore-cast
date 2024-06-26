# Data with focus on major entry points
# Load necessary libraries
library(dplyr)
library(lubridate)

# Load your province data (assuming province_data is already loaded)
# province_data <- fread("/mnt/data/province_data.csv")  # Uncomment and replace with actual path if needed

# Country aggregate data by weeks, ensuring the end of the week is reflected (Sunday)
province_data_weekly <- province_data %>% 
  mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
  group_by(week) %>%
  summarise(weekly_incidence = sum(incidence, na.rm = TRUE))

# Weekly data ending on Friday
province_data_weekly_friday <- province_data %>% 
  mutate(week_ending_friday = floor_date(as.Date(date), "week") + days(4)) %>%
  group_by(week_ending_friday) %>%
  summarise(weekly_incidence_fri = sum(incidence, na.rm = TRUE))

# Fill in NAs for the rest of the week
province_data_filled <- province_data %>%
  mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
  group_by(week) %>%
  mutate(weekly_incidence = ifelse(date == max(date), sum(incidence, na.rm = TRUE), NA))

# Weekly aggregated data by province
provinces <- unique(province_data$province)
provinces <- provinces[provinces != "unknown"]

province_list <- c()

for (province in provinces) {
  province_list[[province]] <- province_data %>% filter(province == !!province)
}

# Calculate weekly incidence for each province and save the results
weekly_incidence_list <- list()

for (province in provinces) {
  province_data <- province_list[[province]]
  
  weekly_incidence <- province_data %>%
    mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
    group_by(week) %>%
    mutate(weekly_incidence = ifelse(date == max(date), sum(incidence, na.rm = TRUE), NA))
    
  variable_name <- gsub(" ", "_", province)  # Replace spaces with underscores
  
  assign(paste0(variable_name, "_weekly_incidence"), weekly_incidence)
}