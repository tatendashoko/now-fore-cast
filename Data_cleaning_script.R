#packages

pacman::p_load(here,
               tidyverse,
               janitor)

# DATA

#WHO data
WHO_COVID_19_global_data <- read_csv(here("data/WHO-COVID-19-global-data.csv")) #Global Covid data

south_africa_covid_19 <- WHO_COVID_19_global_data %>% 
  filter(Country == "South Africa") %>% 
  select(Date_reported, Country_code, New_cases, Cumulative_cases)

describe(south_africa_covid_19)


## NICD data

province_data <- read_csv(here("data/province_data.csv"))

require(data.table)

setDT(province_data)

province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]

(province_data[province_id != "U"] |> ggplot()) + aes(date, incidence, color = province_id) +
  geom_line() +
  scale_x_date() + scale_y_log10() + theme_minimal()



