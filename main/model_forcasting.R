model <- function(data, generation_time, delay, rt_prior, pred_window){
  obs_stuff <- obs_opts(
  family = c("negbin", "poisson"),
  phi = list(mean = 0, sd = 1),
  weight = 1,
  week_effect = TRUE,
  week_length = 7,
  scale = 1,
  na = "accumulate",
  likelihood = TRUE,
  return_likelihood = FALSE
)
return(
    epinow(data,
    generation_time = generation_time_opts(generation_time),
    obs = obs_stuff,
    delays = delay_opts(delay),
    rt = rt_opts(prior = rt_prior),
    horizon = pred_window,
    verbose = FALSE
    )
)
} 

## Daily Parameters 
incubation_period <- LogNormal(mean = 5, sd = 1, max = 14)
generation_time <- Gamma(mean = 7.12, sd = 1.72, max=10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)

delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 3, sd = 0.1)

## Weekly Parameters
# converting days to weeks
# weekly_incubation_period <- LogNormal(mean = 5/7, sd = 1/7, max = 2) 
# weekly_generation_time <- Gamma(mean = 7.12/7, sd = 1.72/7, max=10/7) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
# weekly_reporting_delay <- LogNormal(mean = 2/7, sd = 1/7, max = 10/7)

# weekly_delay <- weekly_incubation_period + weekly_reporting_delay
