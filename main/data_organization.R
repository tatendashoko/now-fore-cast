#packages

pacman::p_load(here,
               rstan,
               tidyverse,
               janitor,
               EpiNow2,
               data.table,
               scoringutils
               )

options(mc.cores=4)

require(data.table)
library("EpiNow2")
library("rstan")
library("scoringutils")

setDT(province_data)

province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]

(province_data[province_id != "U"] |> ggplot()) + aes(date, incidence, color = province_id) +
  geom_line() +
  scale_x_date() + scale_y_log10() + theme_minimal()

# province_data_filtered <- select(province_data, c('date', 'province', 'incidence', 'cumulative_cases'))
# reported_cases <- select(province_data_filtered[1:60], c("date"="Date", "incidence"="Cases"))

province_data_filtered <- province_data %>%
  filter(province != "unknown") %>%
  select(date, province, incidence, cumulative_cases)


incubation_period <- LogNormal(mean = 5, sd = 1, max = 14)
generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 2, sd = 0.1)

reported_province_cases <- function(province_name, start_day=1, end_day){
  cases <- province_data_filtered %>%
    filter(province == province_name) %>%
    slice(start_day:end_day) %>%
    select(date, incidence) %>%
    rename(confirm = incidence)
  return(cases)
}