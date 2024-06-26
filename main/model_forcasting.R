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

# DATA

## NICD data
require(data.table)
library("EpiNow2")
library("rstan")
library("scoringutils")

setDT(province_data)
options(mc.cores = 4)

province_data[
  order(date),
  incidence := c(cumulative_cases[1], diff(cumulative_cases)),
  by = province_id
]

province_data_filtered <- province_data %>%
  filter(province != "unknown") %>%
  select(date, province, incidence, cumulative_cases)

reported_province_cases <- function(province, start_day=1, type="daily"){
  if(type == "daily"){
    cases <- province_data_filtered %>%
        filter(province == province) %>%
        slice(start_day:end_day) %>%
        select(date, incidence) %>%
        rename(confirm = incidence)
  }else {
      cases <- province %>%
        ungroup() %>%
        slice(start_day:end_day) %>%
        rename(confirm = weekly_incidence) %>%
        select(date, confirm)
  }
  return(cases)
}

model <- function(data,generation_time, delay, rt_prior, pred_window){
return(
    epinow(data,
    generation_time = generation_time_opts(generation_time),
    delays = delay_opts(delay),
    rt = rt_opts(prior = rt_prior),
    horizon = pred_window,
    )
)
} 

## Daily Parameters 
daily_incubation_period <- LogNormal(mean = 5, sd = 1, max = 14)
daily_generation_time <- Gamma(mean = 7.12, sd = 1.72, max=10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
daily_reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

daily_delay <- daily_incubation_period + daily_reporting_delay
rt_prior <- list(mean = 3, sd = 0.1)

## Weekly Parameters
# converting days to weeks
weely_incubation_period <- LogNormal(mean = 5/7, sd = 1/7, max = 2) 
weely_generation_time <- Gamma(mean = 7.12/7, sd = 1.72/7, max=10/7) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
weely_reporting_delay <- LogNormal(mean = 2/7, sd = 1/7, max = 10/7)

weely_delay <- weely_incubation_period + weely_reporting_delay
