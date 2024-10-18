library(data.table)
library(ggplot2)
library(patchwork)

#' Get saved local/output and combine into a single dt
#'
#' @param forecast_type Forecast type. Options: c("daily", "weekly")
#' @param result_type Result type. Options: c("timing", "forecast", "diagnostics")
#'
#' @return A data.table of the results by province and forecast type
#' @export
#'
#' @examples
#' TODO: Enhance to deal with diagnostics data
combine_results <- function(forecast_type = c("daily", "weekly"),
                            result_type = c("timing", "forecast", "diagnostics")
                            ){
    # Get the outputs
    outputs <- file.path(
		"local/output",
		list.files("local/output", paste0(".*_", forecast_type, "(.*).rds"))
	)
	# Get province names
	provinces_included <- gsub(".*_(.*)\\.rds$", "\\1", outputs)
	
	# get results and add names
	results_combined <- lapply(
		as.list(outputs),
		function(x) {
			readData <- ifelse(result_type == "diagnostics", fread, readRDS)
			# x is a nested dt of forecasts, timing, and diagnostics for a single slide
			dt <- readData(x)
			dt <- dt |> 
				subset(select = result_type)
			dt_fin <- rbindlist(dt[[result_type]])
		}
	)
	# Annotate the combined runtimes
	names(results_combined) <- provinces_included
	
	# Combine the files
	res <- results_combined |>
		rbindlist(idcol = "province")
	# Annotate with the frequency
	res$frequency <- forecast_type
	return(res)
}


# Plots
# Cases
daily_cases <- readRDS(file.path("local", "data", "daily_EC.rds"))
weekly_cases <- readRDS(file.path("local", "data", "weekly_EC.rds"))

train_window <- 70

# Dates for completing the generated quantities (scores, diagnostics, and timing) by adding the dates of the training window
complete_dates_dt <- daily_cases[, .(date)]

# cases plot
cases_plt <- ggplot() +
	geom_point(
		aes(x = date, y = confirm),
		data = daily_cases, size = 0.5
	) +
	geom_segment(
		aes(x = date - 6.5, xend = date + 0.5, y = confirm/7, yend = confirm/7),
		data = weekly_cases[!is.na(confirm)],
		color = "firebrick"
	) +
	scale_y_continuous(
		"Daily Incidence", sec.axis = sec_axis(
			~ . * 7, name = "Weekly Incidence" 
		)
	)

cases_plt

########## 
# Scores
##########

scores <- readRDS("local/output/score_EC.rds")

## Add score types
scores <- scores[, score_type := paste0(from, "_forect_vs", "_", to, "_data")]

scores_complete <- merge(complete_dates, scores, by = "date", all.x = TRUE)

# scores_reshaped <- scores |> 
# 	melt(
# 	id.vars = c("slide", "date", "crps"),   # Columns to keep
# 	measure.vars = c("from", "to"),         # Columns to reshape
# 	variable.name = "type",                 # New column indicating 'from' or 'to'
# 	value.name = "frequency"                # New column for the values of 'from' and 'to'
# )

# scores_dt <- scores[, slide := slide + train_window] # Shift slides by train window

# scores plot
score_plt <- ggplot(data = scores_complete) +
	geom_line(
	    aes(x = date,
	        y = crps,
	        color = from,
	        linetype = to
	    )
	) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_y_log10()
score_plt

## Run times
## Daily
# runtimes_daily_dt <- combine_results("daily", "timing")
# # Weekly data
# runtimes_weekly_dt <- combine_results("weekly", "timing")
# 
# # Combine the daily and weekly runtimes
# timing_dt_combined <- rbind(runtimes_daily_dt, runtimes_weekly_dt)
# # Add dates
# timing_dt_with_dates <- merge(timing_dt_combined, scores[, .(slide, date)], by = "slide", all.x = TRUE, allow.cartesian = TRUE)
# 
# timing_dt_with_dates <- timing_dt_with_dates[, slide := slide + train_window] # Shift slides by train window
# 
# # Round times
# timing_dt_with_dates <- timing_dt_with_dates[, timing := round(lubridate::as.duration(timing), 1)]
# # reshape the data to wide
# timing_dt_reshaped <- timing_dt_with_dates |>
#     dcast(province + slide ~ frequency, value.var = "timing")
# 
# # Add categorical indicator of comparison
# timing_dt_reshaped <- timing_dt_reshaped[, daily_greater := ifelse(daily > weekly, "yes", "no")]
# 
# ## Runtimes plot
# runtimes_plt <- ggplot(data = timing_dt_combined[province == "EC", ]) +
# 	geom_point(
# 		aes(x = slide,
# 				y = timing,
# 				shape = frequency
# 				)
# 	) +
# 	geom_linerange(
# 		data = timing_dt_reshaped[province == "EC", ],
# 		aes(x = slide,
# 				ymin = daily,
# 				ymax = weekly,
# 				color = daily_greater
# 		)
# 	) +
# 	scale_y_continuous(sec.axis = sec_axis(~ . + 10000)) +
# 	# coord_flip() +
# 	facet_wrap(~province) +
# 	theme_bw()
# 
# runtimes_plt

### Diagnostics
# Get diagnostics files
diagnostics_csvs <- file.path("local/output", list.files("local/output", ".csv"))
diagnostics <- lapply(as.list(diagnostics_csvs), fread)

# Annotate with province names
diagnostics_provinces <- gsub(".*_(.*)\\.csv$", "\\1", diagnostics_csvs)
names(diagnostics) <- diagnostics_provinces

# Combine the files
diagnostics_dt <- diagnostics |> rbindlist(idcol = "province")

diagnostics_dt <- merge(scores_complete[, .(date, slide)], diagnostics_dt[province == "EC"], by = "slide", all.x = TRUE, allow.cartesian = TRUE)

diagnostics_dt <- diagnostics_dt[, slide := slide + train_window] # Shift slides by train window

divergences_plt <- ggplot(data = diagnostics_dt,
                          aes(x = date.x,
                              y = divergent_transitions,
                              fill = type
                          )
) +
    geom_line() +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

divergences_plt

# Patchwork
(cases_plt /
        score_plt /
        divergences_plt
    ) &
    theme_minimal() &
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") &
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 