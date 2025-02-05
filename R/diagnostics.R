library(data.table)

.args <- if (interactive()) {
	c(
		file.path("local/output", c("forecast_daily_EC.rds", "forecast_weekly_EC.rds")),
		file.path("local/output", "diagnostics_EC.csv")
	)
} else {
	commandArgs(trailingOnly = TRUE)
}

# Get diagnostic data
diagnostics_daily_fits <- readRDS(.args[1])$diagnostics |> rbindlist()
diagnostics_weekly_fits <- readRDS(.args[2])$diagnostics |> rbindlist()

# Create groupable data for the plots
diagnostics_daily_fits$type <- "daily"
diagnostics_weekly_fits$type <- "weekly"
diagnostics_dt_combined <- rbind(diagnostics_daily_fits, diagnostics_weekly_fits)

# Calculate ESS per second
diagnostics_dt_combined[, `:=`(
    ess_basic_ps = ess_basic/stan_elapsed_time,
    ess_bulk_ps = ess_bulk/stan_elapsed_time,
    ess_tail_ps = ess_tail/stan_elapsed_time)
]

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
