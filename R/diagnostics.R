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
files <- .args[1:3]
# Extract the forecast target
targets_labels <- gsub("^([^_]+)_([^_]+)_([^.]+)\\.rds$", "\\2", files)
# Replace "special" with "rescale"; old name -> new name
target_labels <- ifelse(targets_labels == "special", "rescale", targets_labels)

diagnostics_dt_combined <- files |>
    setNames(target_labels) |>
    lapply(readRDS) |>
    lapply(\(obj) {
        obj$diagnostics |> rbindlist()
    }) |>
    rbindlist(idcol = "type", fill = TRUE)

# Save as csv
write.csv(diagnostics_dt_combined, tail(.args, 1), row.names = FALSE)
