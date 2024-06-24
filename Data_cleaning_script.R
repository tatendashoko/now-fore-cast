#packages

pacman::p_load(here,
               tidyverse,
               janitor)

# DATA

WHO_COVID_19_global_data <- read_csv(here("data/WHO-COVID-19-global-data.csv"))