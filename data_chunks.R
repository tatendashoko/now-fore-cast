
# Country aggregate data by weeks
province_data_weekly <- province_data %>% 
  mutate(week = floor_date(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(weekly_incidence= sum(incidence))

#friday
province_data_weekly_friday <- province_data %>% 
  mutate(week_ending_friday = ceiling_date(as.Date(date), "week") - days(2)) %>%
  group_by(week_ending_friday) %>%
  summarise(weekly_incidence_fri = sum(incidence))

# Weekly aggregated data by province

provinces <- unique(province_data$province)
provinces <- provinces[-1]

province_list <- c()

for (province in provinces) {
  province_list[[province]] <- province_data %>% filter(province == !!province)
}

for (province in provinces) {
  print(province)
  assign(province, as_data_frame(province_list[[province]]))
}

# Save each province's data frame to a separate CSV file
for (province in provinces) {
  # Create a variable name from the province name
  variable_name <- gsub(" ", "_", province)  # Replace spaces with underscores
  # Save the dataframe to a CSV file
  write.csv(province_list[[province]], paste0("data/",variable_name, ".csv"), row.names = FALSE)
}

# Calculate weekly incidence for each province and save the results
weekly_incidence_list <- list()

for (province in provinces) {
  province_data <- province_list[[province]]
  
  weekly_incidence <- province_data %>%
    mutate(week = floor_date(as.Date(date), "week")) %>%
    group_by(week) %>%
    summarise(weekly_incidence = sum(cumulative_cases, na.rm = TRUE))
  
  variable_name <- gsub(" ", "_", province)  # Replace spaces with underscores
  
  assign(paste0(variable_name, "_weekly_incidence"), weekly_incidence)
  
}

# Weekly aggregated by whole country


# Data with focus on major entry points