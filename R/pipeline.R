
library(EpiNow2)
library(data.table)
library(parallel)

.args <- if (interactive()) c(
  "data/weekly_EC.rds",
  "output/forecast_weekly_EC.rds"
) else commandArgs(trailingOnly = TRUE)

# inflate as.Date, because EpiNow2 seems to prefer Date over IDate
dt <- readRDS(.args[1])[, .(date = as.Date(date), confirm)]

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

so <- stan_opts(
#	samples = 10,
	cores = parallel::detectCores()
)

# slides <- slides[1:5]
# find the position of first non-zero
# keep it, any leading NAs
trim_leading_zero <- function (init_dt) { 
	first_non_zero <- init_dt[, which.max(confirm != 0)]
	if (first_non_zero == 1) {
		return(rbind(init_dt[1, .(date = date - 1, confirm = 0)], init_dt))
	} else {
		# get all the zeros
		zeros <- init_dt[, c(which(confirm == 0), .N)]
		# find the last one before the first non-zero
		from <- which.max(zeros > first_non_zero) - 1L
		if (from == 0) {
			return(rbind(init_dt[1, .(date = date - 1, confirm = 0)], init_dt))
		} else {
			return(init_dt[zeros[from]:.N])
		}
	}
}

res_dt <- lapply(slides, \(slide) {
	slice <- dt[seq_len(train_window) + slide] |> trim_leading_zero()
	if (slice[, .N > test_window * 2]) {
	  epinow(
	    data = slice,
	    generation_time = generation_time_opts(generation_time),
	    delays = delay_opts(delay), rt = rt_opts(prior = rt_prior),
	    horizon = test_window, obs = obs, logs = NULL, stan = so
	  )$estimates$samples[variable == "reported_cases" & type == "forecast", .(date, sample, value, slide = slide)]
	} else data.table(
		date = dt[train_window + slide, date + seq_len(test_window)],
		sample = NA_integer_, value = NA_integer_, slide = slide
	)
}) |> rbindlist()

res_dt |> saveRDS(tail(.args, 1))