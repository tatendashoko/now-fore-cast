
library(EpiNow2)
library(data.table)
library(parallel)

.args <- if (interactive()) c(
  "data/daily_EC.rds",
  "output/forecast_daily_EC.rds"
) else commandArgs(trailingOnly = TRUE)

options(mc.cores = detectCores()-1L)

dt <- readRDS(.args[1])[, .(date, confirm)]

train_window <- 7*10
test_window <- 7*2

slides <- seq(0, dt[, .N - (train_window + test_window)], by = test_window)

incubation_period <- LogNormal(mean = 5, sd = 1, max = 14)
# generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
generation_time <- Gamma(mean = 7.12, sd = 1.72, max = 10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
reporting_delay <- LogNormal(mean = 2, sd = 1, max = 10)
delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 3, sd = 0.1)

obs <- obs_opts(
  family = c("negbin", "poisson"),
  phi = list(mean = 0, sd = 1),
  weight = 1,
  week_effect = FALSE,
  week_length = 7,
  scale = 1,
  na = "accumulate",
  likelihood = TRUE,
  return_likelihood = FALSE
)

# TODO: remove once done testing
so <- stan_opts(
#  samples = 100
)
# slides <- slides[1:5]


trim_leading_zero <- function (dt) { dt[which.min(confirm != 0):.N] }

res_dt <- lapply(slides, \(slide) {
  epinow(
    data = dt[(1:train_window)+slide] |> trim_leading_zero(),
    generation_time = generation_time_opts(generation_time),
    delays = delay_opts(delay), rt = rt_opts(prior = rt_prior),
    horizon = test_window, obs = obs, logs = NULL, stan = so
  )$estimates$samples[variable == "reported_cases" & type == "forecast", .(date, sample, value, slide = slide)]
}) |> rbindlist()

res_dt |> saveRDS(tail(.args, 1))