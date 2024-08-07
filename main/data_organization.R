# DATA

## NICD data

province_data_raw <- read_csv("data/province_data.csv") %>% 
  clean_names()

province_data <- setDT(province_data_raw)


correct_cumulative_cases <- function(cumulative_cases) {
   corrected_cases <- cumulative_cases
  n <- length(corrected_cases)
  for (i in 2:(n-1)) {
    if ((corrected_cases[i - 1] > corrected_cases[i + 1]) & (corrected_cases[i] > corrected_cases[i - 1])){
      corrected_cases[i+1] <- corrected_cases[i]
    } else if ((corrected_cases[i] < corrected_cases[i - 1]) & (corrected_cases[i] < corrected_cases[i + 1])) {
      corrected_cases[i] <- corrected_cases[i - 1]
    } else if ((corrected_cases[i] > corrected_cases[i - 1]) & (corrected_cases[i] > corrected_cases[i + 1])) {
      corrected_cases[i] <- corrected_cases[i - 1]
    }
  }
  # Handle the last element separately if needed
  if (n > 1 && corrected_cases[n] < corrected_cases[n - 1]) {
    corrected_cases[n] <- corrected_cases[n - 1]
  }
  return(corrected_cases)
}

# Apply the correction function
province_data <- province_data %>%
  group_by(province) %>%
  mutate(cumulative_cases = correct_cumulative_cases(cumulative_cases)) %>%
  ungroup()

province_data <- setDT(province_data)
province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]

(province_data[province_id != "U"] |> ggplot()) + aes(date, incidence, color = province_id) +
  geom_line() +
  scale_x_date() + scale_y_log10() + theme_minimal()


# Country aggregate data by weeks, ensuring the end of the week is reflected (Sunday)
province_data_weekly <- function(province_data) {
  aggregated <- province_data %>% 
    mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
    group_by(week) %>%
    summarise(weekly_incidence = sum(incidence, na.rm = TRUE))
}

# # Weekly data ending on Friday
province_data_weekly_friday <- function(province_data) {
  aggregated <- province_data %>% 
    mutate(week_ending_friday = floor_date(as.Date(date), "week") + days(4)) %>%
    group_by(week_ending_friday) %>%
    summarise(weekly_incidence_fri = sum(incidence, na.rm = TRUE))
}

# # Fill in NAs for the rest of the week
province_data_filled <- function(province_data) {
  aggregated <- province_data %>%
    mutate(week = floor_date(as.Date(date), "week") + days(6)) %>%
    group_by(week) %>%
    mutate(weekly_incidence = ifelse(date == max(date), sum(incidence, na.rm = TRUE), NA))
}

# Weekly aggregated data by province
provinces <- unique(province_data$province)
provinces <- provinces[provinces != "Unknown"]
provinces <- provinces[provinces != "Northern Cape"]
provinces <- provinces[provinces != "Eastern Cape"]

province_list <- c()

weekly_incidence_list <- list()

for (province in provinces) {
  province_list[[province]] <- province_data %>% filter(province == !!province)
  assign(gsub(" ", "_", province), as_data_frame(province_list[[province]]))
  
  weekly_incidence <- province_data_filled(province_list[[province]])

  variable_name <- gsub(" ", "_", province)  # Replace spaces with underscores
  
  assign(paste0(variable_name, "_weekly_incidence"), weekly_incidence)
}

national_data <- province_data %>%
  filter(province != "Unknown") %>%
  group_by(date) %>%
  summarize(incidence = sum(incidence, na.rm = TRUE))

weekly_aggregated_data <- province_data_filled(province_data) %>%
  filter(province != "Unknown") %>%
  mutate(date = floor_date(as.Date(date), "week") + days(6)) %>%
  group_by(date) %>%
  summarize(incidence = sum(incidence, na.rm = TRUE)) %>%
  ungroup()

# Find the start of the first week
first_day <- floor_date(min(weekly_aggregated_data$date), "week")

weekly_national_data <- weekly_aggregated_data %>%
  complete(date = seq.Date((first_day + 1), max(date), by = "day")) %>%
  mutate(weekly_incidence = if_else(wday(date) == 7, incidence, NA_real_))

national_total_by_province <- province_data %>%
  filter(province != "Unknown") %>%
  group_by(province) %>%
  summarize(incidence = sum(incidence, na.rm = TRUE))
