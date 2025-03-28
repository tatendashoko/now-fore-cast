library(data.table)

.args <- if (interactive()) {
	c(
	    file.path("local/output",
	              c("forecast_daily_GP.rds",
	                "forecast_weekly_GP.rds",
	                "forecast_special_GP.rds"
	              )
	    ),
	    file.path("local/output", "diagnostics_GP.csv")
	)
} else {
    commandArgs(trailingOnly = TRUE)
}

# Get diagnostic data
.args[1:3] |> setNames(c("daily", "weekly", "rescale")) |>
  lapply(readRDS) |> lapply(\(obj) {
    obj$diagnostics |> rbindlist()
}) |> rbindlist(idcol = "type", fill = TRUE)

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

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
