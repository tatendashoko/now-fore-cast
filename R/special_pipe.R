library(EpiNow2)
library(data.table)
library(parallel)
library(bayesplot)

.args <- if (interactive()) c(
  "local/data/weekly_EC.rds",
  "local/output/forecast_special_EC.rds"
) else commandArgs(trailingOnly = TRUE)

# inflate as.Date, because EpiNow2 seems to prefer Date over IDate
dt <- readRDS(.args[1])[, .(date = as.Date(date), confirm)][!is.na(confirm)]

# EpiNow wants to work in terms of days, so we're going to pretend
# as if weeks are days

orig_dates <- dt$date
new_dates <- seq.Date(
  from = orig_dates[1],
  by = "day",
  length.out = length(orig_dates)
)
dt$date <- new_dates

train_window <- 10
test_window <- 2

slides <- seq(0, dt[, .N - (train_window + test_window)], by = test_window)

# when changing units, the mean, sd, and max scale the same way
incubation_period <- LogNormal(mean = 5 / 7, sd = 1 / 7, max = 14 / 7)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
# generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
generation_time <- Gamma(mean = 7.12 / 7, sd = 1.72 / 7, max = 10 / 7)
reporting_delay <- LogNormal(mean = 2 / 7, sd = 1 / 7, max = 10 / 7)
delay <- incubation_period + reporting_delay
rt_prior <- list(mean = 1, sd = 0.5)

# Observation model
obs <- obs_opts(
  week_effect = FALSE,
  na = "missing", # shouldn't matter for this case
  likelihood = TRUE,
  return_likelihood = FALSE
)

so <- stan_opts(
	cores = parallel::detectCores() - 2,
	samples = 5000,
	control = list(adapt_delta = 0.999, stepsize = 0.1)
)

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

#' @title Get rstan diagnostics
#' @description
#' Summarise the diagnostic information contained in a `<stanfit>` object. If
#' the object is not a stanfit object, return a data.table with NA values.
#' This function is adapted from the `{epidist}` R package in
#' https://github.com/epinowcast/epidist/pull/175/files
#' 
#' @param fit A stanfit object
#'
#' @return A data.table containing the summarised diagnostics
get_rstan_diagnostics <- function(fit) {
	if (inherits(fit, "stanfit")) {
		np <- bayesplot::nuts_params(fit)
		divergent_indices <- np$Parameter == "divergent__"
		treedepth_indices <- np$Parameter == "treedepth__"
		diagnostics <- data.table(
			"samples" = nrow(np) / length(unique(np$Parameter)),
			"max_rhat" = round(max(bayesplot::rhat(fit), na.rm = TRUE), 3),
			"divergent_transitions" = sum(np[divergent_indices, ]$Value),
			"per_divergent_transitions" = mean(np[divergent_indices, ]$Value),
			"max_treedepth" = max(np[treedepth_indices, ]$Value)
		)
		diagnostics[, no_at_max_treedepth :=
									sum(np[treedepth_indices, ]$Value == max_treedepth)
		][, per_at_max_treedepth := no_at_max_treedepth / samples]
	} else{
		diagnostics <- data.table(
			"samples" = NA,
			"max_rhat" = NA,
			"divergent_transitions" = NA,
			"per_divergent_transitions" = NA,
			"max_treedepth" = NA,
			"no_at_max_treedepth" = NA,
			"per_at_max_treedepth" = NA
		)
	}
	return(diagnostics[])	
}

res_dt <- lapply(slides, \(slide) {
	slice <- dt[seq_len(train_window) + slide] |> trim_leading_zero()
	if (slice[, .N > test_window * 2]) {
		out <- epinow(
			data = slice,
			generation_time = generation_time_opts(generation_time),
			delays = delay_opts(delay),
			rt = rt_opts(prior = rt_prior),
			horizon = test_window,
			obs = obs,
			stan = so
		)
		# Extract the forecasted cases
		forecasts <- out$estimates$samples[
			variable == "reported_cases" & type == "forecast",
			.(date, sample, value, slide = slide)
			]
		# Extract the diagnostic information
		diagnostics <- get_rstan_diagnostics(out$estimates$fit)[, slide := slide]
		# Extract the timing information
		run_time <- out$timing
		# Combine the forecast, timing and diagnostics
		forecast_dt <- data.table(
			forecast = list(forecasts),
			timing = list(
				data.table(slide = slide, timing = run_time)
			),
			diagnostics = list(diagnostics),
			fit = ifelse(run_time < lubridate::duration(5), list(out$estimates$fit), list(NA)) # Only save the fit if the runtime is less than 5 secs (for memory reasons; the fits are massive = 27 Gb ish)
		)
	} else {
		empty_forecast <- data.table(
			date = dt[train_window + slide, date + seq_len(test_window)],
			sample = NA_integer_, value = NA_integer_, slide = slide
		)
		res <- data.table(
			forecast = list(empty_forecast),
			timing = list(data.table(
				slide = slide,
				timing = lubridate::as.duration(NA))
				),
			diagnostics = list(data.table(
				slide = slide,
				"samples" = NA,
				"max_rhat" = NA,
				"divergent_transitions" = NA,
				"per_divergent_transitions" = NA,
				"max_treedepth" = NA,
				"no_at_max_treedepth" = NA,
				"per_at_max_treedepth" = NA
			),
			fit = list(NA)
			)
		)
	}
}) |> rbindlist()

## TODO: reach into res_dt and update forecast using orig_dates

res_dt |> saveRDS(tail(.args, 1))
