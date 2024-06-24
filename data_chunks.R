
# Country aggregate data by weeks
province_data_weekly <- province_data %>% 
  mutate(week = floor_date(as.Date(date), "week")) %>%
  group_by(week) %>%
  summarise(weekly_incidence= sum(incidence))

province_data_weekly_friday <- province_data %>% 
  mutate(week_ending_friday = ceiling_date(as.Date(date), "week") - days(2)) %>%
  group_by(week_ending_friday) %>%
  summarise(weekly_incidence_fri = sum(incidence))

# Weekly aggregated data by province

# Weekly aggregated by whole country


# Data with focus on major entry points