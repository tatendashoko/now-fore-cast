library(data.table)

.args <- if (interactive()) {
	c(
		file.path("local/output", c("forecast_daily_GP.rds", "forecast_weekly_GP.rds", "forecast_special_GP.rds")),
		file.path("local/output", "diagnostics_GP.csv")
	)
} else {
    commandArgs(trailingOnly = TRUE)
}

# Get diagnostic data
daily_dt <- readRDS(.args[1])$diagnostics |> rbindlist()
weekly_dt <- readRDS(.args[2])$diagnostics |> rbindlist()
special_dt <- readRDS(.args[3])$diagnostics |> rbindlist()

# Create groupable data for the plots
daily_dt$type <- "daily"
weekly_dt$type <- "weekly"
special_dt$type <- "rescale"
diagnostics_dt_combined <- rbind(
    daily_dt,
    weekly_dt,
    special_dt,
    fill = TRUE
)

# Calculate ESS per second
# diagnostics_dt_combined[, `:=`(
#     ess_basic_ps = ess_basic/stan_elapsed_time,
#     ess_bulk_ps = ess_bulk/stan_elapsed_time,
#     ess_tail_ps = ess_tail/stan_elapsed_time)
# ]

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
