library(EpiNow2)
library(epiparameter)
library(data.table)
library(parallel)
library(bayesplot)

.args <- if (interactive()) c(
  "local/data/weekly_EC.rds",
  "local/output/TODO.rds"
) else commandArgs(trailingOnly = TRUE)

# inflate as.Date, because EpiNow2 seems to prefer Date over IDate
dt <- readRDS(.args[1])[, .(date = as.Date(date), confirm)]

# EpiNow wants to work in terms of days, so we're going to pretend
# as if weeks are days
rescale_dt <- dt[!is.na(confirm)][, orig_date := date]

train_window <- 7*10
test_window <- 7*2

slides <- seq(0, dt[, .N - (train_window + test_window)], by = test_window)
rescale_slides <- seq(0, rescale_dt[, .N - (train_window/7 + test_window/7)], by = test_window/7)

# TODO @james pick out some useful ones?
SOMENUMBERS <- c(1, 2, 3, 4) 

target_slides <- slides[SOMENUMBERS]
target_slides_rescale <- rescale_slides[SOMENUMBERS]

# Incubation period
# Get from epiparameter package
sars_cov_incubation_dist <- epiparameter_db(
    disease = "COVID-19",
    single_epiparameter = TRUE,
    epi_name = "incubation period"
)

incubation_max <- round(quantile(sars_cov_incubation_dist, 0.999)) # Upper 99.9% range needed for EpiNow2

incubation_period <- LogNormal(
    meanlog = sars_cov_incubation_dist[["meanlog"]],
    sdlog = sars_cov_incubation_dist[["sdlog"]],
    max = incubation_max
) # doi:10.3390/jcm9020538 obtained from epiparameter by running 

# TODO @james
incubation_period_rescale <- ...

# Generation period
generation_time <- Gamma(mean = 7.12, sd = 1.72, max = 10) # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9837419/
generation_time_rescale <- Gamma(mean = 7.12/7, sd = 1.72/7, max = 10/7)

# Reporting delays
# TODO @james fix the rescale - also, why is the regular one in meanlog and sd, rather than meanlog, sdlog?
reporting_delay <- LogNormal(meanlog = 0.58, sd = 0.47, max = 10) # mean = 2, sd = 1
reporting_delay_rescale <- LogNormal(meanlog = 0.58, sd = 0.47, max = 10) # mean = 2, sd = 1

# Total delays
delay <- incubation_period + reporting_delay
delay_rescale <- incubation_period_rescale + reporting_delay_rescale

# Rt prior
rt_prior <- LogNormal(meanlog = 0.69, sdlog = 0.05) # mean = 2, sd = 0.1

# Observation models
obs_daily <- obs_opts(
  week_effect = TRUE,
  na = "missing",
  likelihood = TRUE,
  return_likelihood = FALSE
)

obs_weekly <- obs_opts(
    week_effect = FALSE,
    na = "accumulate",
    likelihood = TRUE,
    return_likelihood = FALSE
)

obs_rescale <- obs_opts(
    week_effect = FALSE,
    na = "missing",
    likelihood = TRUE,
    return_likelihood = FALSE
)

base_stan_opts <- stan_opts(
	cores = parallel::detectCores() - 2,
	samples = 5000,
	stepsize = 1, # stan default
	adapt_delta = 0.8 # stan default, diff from EpiNow2 default of 0.9
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

update_stan_opts <- function(cur_opts, delta_inc = 0.8, step_inc = 0.5) {
    cur_opts$control <- within(cur_opts$control, {
        adapt_delta <- (1 - adapt_delta)*0.8 + adapt_delta
        stepsize <- stepsize*(1 - step_inc)
    })
    return(cur_opts)
}

# TODO @james should return TRUE if meeting successfully-fit-criteria, FALSE otherwise
check_passing <- function(diagnos) {
    
}

loop_slice <- function(
    slide, test_window, train_window, dt,
    gen_time, delay_time, obs,
    ATTEMPTS_MAX = 5
) {
    slice <- dt[seq_len(train_window) + slide] |> trim_leading_zero()
    
    passed_diagnostics <- FALSE
    attempts <- 0
    local_stan_opts <- base_stan_opts
    total_stan_time <- 0
    total_crude_time <- 0
    
    gt_opts <- generation_time_opts(gen_time)
    d_opts <- delay_opts(delay_time)
    
    while (!passed_diagnostics && (attempts < ATTEMPTS_MAX)) {
        attempts <- attempts + 1
        # Fit the model
        out <- epinow(
            data = slice,
            generation_time = gt_opts,
            delays = d_opts,
            rt = rt_opts(prior = rt_prior),
            horizon = test_window,
            obs = obs,
            stan = local_stan_opts
        )

        # Extract the diagnostic information
        diagnostics <- get_rstan_diagnostics(out$estimates$fit)
        # Extract and append stan's internal timing of the model fitting process.
        stan_elapsed_time <- sum(rstan::get_elapsed_time(out$estimates$fit))
        # Extract the crude timing measured by epinow()
        crude_run_time <- out$timing
        
        total_stan_time <- stan_elapsed_time
        total_crude_time <- crude_run_time
        
        
        passed_diagnostics <- check_passing(diagnostics)
        
        if (passed_diagnostics || (attempts == ATTEMPTS_MAX)) {
            # Extract the forecast cases
            forecasts <- out$estimates$samples[
                variable == "reported_cases" & type == "forecast",
                .(date, sample, value, slide = slide)
            ]
            diagnostics <- diagnostics[, slide := slide]
            diagnostics <- diagnostics[, stan_elapsed_time := stan_elapsed_time] #  NB: NEEDS REVIEW: Currently computes total time taken for warmup and sampling for all chains.
            # Combine the forecast, timing and diagnostics
            forecast_dt <- data.table(
                forecast = list(forecasts),
                timing = list(
                    data.table(
                        slide = slide,
                        last_crude_run_time = crude_run_time,
                        last_stan_elapsed_time = stan_elapsed_time,
                        total_crude_time = total_crude_time,
                        total_stan_time = total_stan_time
                    )
                ),
                diagnostics = list(diagnostics),
                fit = ifelse(stan_elapsed_time < lubridate::duration(3), list(out$estimates$fit), list(NA)) # Only save the fit if the runtime is less than specified secs (for memory reasons; the fits are massive = 27 Gb ish)
            )
            return(forecast_dt)
        } else {
            local_stan_opts <- update_stan_opts(local_stan_opts)
        }
        
    }
    
    stop("Should be unreachable")
    
}


res_daily_dt <- lapply(
    target_slides,
    loop_slice,
    test_window = test_window, train_window = train_window, dt = daily_dt,
    gen_time = generation_time, delay_time = delay, obs = obs_daily
) |> rbindlist()

res_weekly_dt <- lapply(
    target_slides,
    loop_slice,
    test_window = test_window, train_window = train_window, dt = weekly_dt,
    gen_time = generation_time, delay_time = delay, obs = obs_weekly
) |> rbindlist()

res_rescale_dt <- lapply(
    target_slides_rescale,
    loop_slice,
    test_window = test_window/7, train_window = train_window/7, dt = rescale_dt,
    gen_time = generation_time_rescale, delay_time = delay_rescale, obs = obs_rescale
) |> rbindlist()



res_dt |> saveRDS(tail(.args, 1))
