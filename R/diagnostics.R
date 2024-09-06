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
daily_dt <- readRDS(.args[1])$diagnostics |> rbindlist()
weekly_dt <- readRDS(.args[2])$diagnostics |> rbindlist()

# Create groupable data for the plots
daily_dt$type <- "daily"
weekly_dt$type <- "weekly"
diagnostics_dt_combined <- rbind(daily_dt, weekly_dt)

# order rows by slide
# diagnostics_dt_combined <- diagnostics_dt_combined[order(slide)]

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
