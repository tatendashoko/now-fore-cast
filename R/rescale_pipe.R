library(EpiNow2)
library(data.table)
library(parallel)
library(bayesplot)

options(mc.cores = parallel::detectCores() - 1)

.args <- if (interactive()) c(
  "local/data/weekly_GP.rds",
  "local/output/forecast_special_GP.rds"
) else commandArgs(trailingOnly = TRUE)

# inflate as.Date, because EpiNow2 seems to prefer Date over IDate
dt <- readRDS(.args[1])[, .(date = as.Date(date), confirm)][!is.na(confirm)]

# EpiNow wants to work in terms of days, so we're going to pretend
# as if weeks are days
dt[, orig_date := date]

fake_daily_dates <- seq.Date(
  from = dt$orig_date[1],
  by = "day",
  length.out = length(dt$orig_date)
)

dt$date <- fake_daily_dates

# Train and forecast windows
train_window <- 10 # 10 weeks
test_window <- 2 # 2 weeks

slides <- seq(0, dt[, .N - (train_window + test_window)], by = test_window)

# when changing units, the mean, sd, and max scale the same way
incubation_period <- LogNormal(mean = 5 / 7, sd = 1 / 7, max = 14 / 7)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7201952/
# generation_time <- LogNormal(mean = 5.2, sd = 1.72, max = 10)
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/

# Generation period
generation_time <- Gamma(mean = 7.12 / 7, sd = 1.72 / 7, max = 10 / 7) # mean and sd are more intuitive for rescaling

# Reporting delays
reporting_delay <- LogNormal(mean = 2 / 7, sd = 1 / 7, max = 10 / 7) # mean and sd are more intuitive for rescaling

# Total delays
delay <- incubation_period + reporting_delay

# Rt prior
rt_prior <- LogNormal(meanlog = 0.69, sdlog = 0.05) # mean = 2, sd = 0.1

# Observation model
obs <- obs_opts(
  week_effect = FALSE,
  likelihood = TRUE,
  return_likelihood = FALSE
)

so <- stan_opts(
	samples = 5000,
	control = list(adapt_delta = 0.999, stepsize = 0.1)
)

# find the position of first non-zero
# keep it, any leading NAs
trim_leading_zero <- function (init_dt) { 
	first_non_zero <- init_dt[, which.max(confirm != 0)]
	if (first_non_zero == 1) {
		return(rbind(init_dt[1, .(date = date - 1, confirm = 0, orig_date = orig_date - 7)], init_dt))
	} else {
		# get all the zeros
		zeros <- init_dt[, c(which(confirm == 0), .N)]
		# find the last one before the first non-zero
		from <- which.max(zeros > first_non_zero) - 1L
		if (from == 0) {
			return(rbind(init_dt[1, .(date = date - 1, confirm = 0, orig_date = orig_date - 7)], init_dt))
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
        # Calculating ESS (basic, bulk, and tail)
        # ESS can only be calculated on the extracted variable in the form of a matrix with dimensions iterations x chains
        # Extract the infections variable as that is used for forecasting
        reports_posterior <- posterior::extract_variable_array(
            posterior::as_draws_array(fit),
            "reports" # NB: NEEDS REVIEW; is it rather infections??
        )
        # Calculate the different types of ess (basic, bulk, and tail)
        fit_ess_basic <- posterior::ess_basic(reports_posterior)
        fit_ess_bulk <- posterior::ess_bulk(reports_posterior)
        fit_ess_tail <- posterior::ess_tail(reports_posterior)

        diagnostics <- data.table(
            "samples" = nrow(np) / length(unique(np$Parameter)),
            "max_rhat" = round(max(bayesplot::rhat(fit), na.rm = TRUE), 3),
            "divergent_transitions" = sum(np[divergent_indices, ]$Value),
            "per_divergent_transitions" = mean(np[divergent_indices, ]$Value),
            "max_treedepth" = max(np[treedepth_indices, ]$Value),
            "ess_basic" = fit_ess_basic,
            "ess_bulk" = fit_ess_bulk,
            "ess_tail" = fit_ess_tail
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
            "per_at_max_treedepth" = NA,
            "ess_basic" = NA,
            "ess_bulk" = NA,
            "ess_tail" = NA,
            "stan_elapsed_time" = NA
        )
    }
    return(diagnostics[])	
}

res_dt <- lapply(slides, \(slide) {
	slice <- dt[seq_len(train_window) + slide] |> trim_leading_zero()
	# Slides for fitting are in weeks but we need to rescale back to
	# days for aligning with other scales
	slide_rescaled <- slide * 7
	# Fit model
	if (slice[, .N > test_window * 2]) {
		out <- epinow(
			data = slice,
			generation_time = generation_time_opts(generation_time),
			delays = delay_opts(delay),
			rt = rt_opts(prior = rt_prior),
			forecast = forecast_opts(horizon = test_window),
			obs = obs,
			stan = so
		)
		# Extract the forecasted cases
		forecasts <- out$estimates$samples[
			variable == "reported_cases" & type == "forecast",
			.(date, sample, value, slide = slide)
			]
		# Extract the diagnostic information
		diagnostics <- get_rstan_diagnostics(out$estimates$fit)
		diagnostics <- diagnostics[, slide := slide_rescaled]
		# Extract and append stan's internal timing of the model fitting process.
		stan_elapsed_time <- sum(rstan::get_elapsed_time(out$estimates$fit))
		diagnostics <- diagnostics[, "stan_elapsed_time" := stan_elapsed_time] #  NB: NEEDS REVIEW: Currently computes total time taken for warmup and sampling for all chains.
		# Extract the crude timing measured by epinow()
		crude_run_time <- out$timing
		# Combine the forecast, timing and diagnostics
		forecast_dt <- data.table(
		    forecast = list(forecasts),
		    timing = list(
		        data.table(
		            slide = slide_rescaled,
		            crude_run_time = crude_run_time,
		            stan_elapsed_time = stan_elapsed_time
		        )
		    ),
		    diagnostics = list(diagnostics)
		)
	} else {
	    empty_forecast <- data.table(
	        date = dt[train_window + slide, date + seq_len(test_window)],
	        sample = NA_integer_, value = NA_integer_, slide = slide_rescaled
	    )
	    res <- data.table(
	        forecast = list(empty_forecast),
	        timing = list(data.table(
	            slide = slide_rescaled,
	            crude_run_time = lubridate::as.duration(NA),
	            stan_elapsed_time = lubridate::as.duration(NA))
	        ),
	        diagnostics = list(data.table(
	            slide = slide_rescaled,
	            "samples" = NA,
	            "max_rhat" = NA,
	            "divergent_transitions" = NA,
	            "per_divergent_transitions" = NA,
	            "max_treedepth" = NA,
	            "no_at_max_treedepth" = NA,
	            "per_at_max_treedepth" = NA,
	            "ess_basic" = NA,
	            "ess_bulk" = NA,
	            "ess_tail" = NA,
	            "stan_elapsed_time" = NA
	        )
	        )
	    )
	}
}) |> rbindlist()

# Reach into res_dt and update forecast as follows:
# - replace the fake dates with orig_dates by doing a merge on date
# - Remove "confirm" and "date" which was fake
res_dt[,
       forecast := lapply(forecast, function(x) {
           merge(x, dt)
       })]

res_dt[,
       forecast := lapply(forecast, function(x) {
           x[, date := orig_date
           ][, `:=`(
               orig_date = NULL,
               confirm = NULL
           )]
       })]

# Save output
res_dt |> saveRDS(tail(.args, 1))
