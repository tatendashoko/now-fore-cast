#packages

pacman::p_load(here,
               tidyverse,
               janitor,
               rstan)

# DATA

## NICD data

province_data_raw <- read_csv(here("data/province_data.csv")) %>% 
  clean_names()

require(data.table)

province_data <- setDT(province_data_raw)

province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]

# (province_data[province_id != "U"] |> ggplot()) + aes(date, incidence, color = province_id) +
#   geom_line() +
#   scale_x_date() + scale_y_log10() + theme_minimal()



