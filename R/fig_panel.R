library(data.table)
library(ggplot2)
library(patchwork)

.args <- if (interactive()) c(
    file.path("local", "data", c("daily_GP.rds", "weekly_GP.rds")), # cases
    file.path("local", "output", "score_GP.rds"), # scores
    file.path("local", "output",
              c("forecast_daily_GP.rds",
                "forecast_weekly_GP.rds",
                "forecast_special_GP.rds"
              )
    ), # forecasts (also contains timing)
    file.path("local", "output", "diagnostics_GP.csv"), # diagnostics
    file.path("local", "figures", "panel_fig_GP.png") # diagnostics
) else commandArgs(trailingOnly = TRUE)

# Load the raw data
# Cases
daily_cases <- readRDS(.args[1])
weekly_cases <- readRDS(.args[2])
# Scores
scores <- readRDS(.args[3])
# Forecasts
forecasts_daily_dt <- readRDS(.args[4])$forecast |>
    rbindlist()
forecasts_daily_dt[, type := "daily"]
forecasts_weekly_dt <- readRDS(.args[5])$forecast |>
    rbindlist()
forecasts_weekly_dt[, type := "weekly"]
forecasts_special_dt <- readRDS(.args[6])$forecast |>
    rbindlist()
forecasts_special_dt[, type := "rescale"]
# Get slide <-> date dictionary
slide_dates_dictionary <- forecasts_weekly_dt[, .SD[1], by = "slide", .SDcols = c("date")]

# Run times
# Daily
runtimes_daily_dt <- readRDS(.args[4])$timing |>
    rbindlist()

runtimes_daily_dt[, type := "daily"]
# Weekly
runtimes_weekly_dt <- readRDS(.args[5])$timing |>
    rbindlist()
runtimes_weekly_dt[, type := "weekly"]
# Rescaled weekly scale
runtimes_special_dt <- readRDS(.args[6])$timing |>
    rbindlist()
runtimes_special_dt[, type := "rescale"]

# Diagnostics
diagnostics_dt <- fread(.args[7])

# Other data
train_window <- 70 # This has to be the same as the one in the pipeline.R script

# We'll need to align the dates in the computed data (scores and diagnostics) with those of the cases data
# when making the panel plot so we'll get the complete dates from the daily cases data
complete_dates_dt <- daily_cases[, .(date)]

#####
#Plots
####

# Cases plot
cases_plt <- ggplot() +
	geom_point(data = daily_cases,
		aes(x = date, y = confirm),
		size = 0.5
	) +
	geom_segment(
		aes(x = date - 6.5, xend = date + 0.5, y = confirm/7, yend = confirm/7),
		data = weekly_cases[!is.na(confirm)],
		color = "firebrick"
	) +
	scale_y_log10(
		"Daily Incidence (log10)", sec.axis = sec_axis(
			~ . * 7, name = "Weekly Incidence (log10)"
		)
	)

cases_plt

########## 
# Scores
##########
## Add missing dates to be able to align with the cases plot panel
scores_complete <- merge(
    complete_dates_dt,
    scores, by = "date", all.x = TRUE
)

# scores plot
score_plt <- ggplot(data = scores_complete) +
	geom_line(
	    aes(x = date,
	        y = crps,
	        color = forecast
	    )
	) +
    geom_point(
        aes(x = date,
            y = crps,
            color = forecast
        )
    ) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_y_log10() +
    scale_color_brewer(na.translate = FALSE, palette = "Dark2") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    facet_wrap(~data, ncol = 1, strip.position = "right") +
    labs(y = "CRPS (log10)",
         linetype = "Data",
         color = "Forecast scale"
    )

score_plt

# Run times
# Daily data
# Add dates by slide
dates_by_slide <- scores[forecast == "daily" & data == "daily"][, .(slide, date)]

runtimes_daily_dt <- runtimes_daily_dt[
    dates_by_slide,
    on = "slide"
]

# Add dates by slide
runtimes_weekly_dt <- runtimes_weekly_dt[
    dates_by_slide,
    on = "slide"
]

runtimes_special_dt <- runtimes_special_dt[
    dates_by_slide,
    on = "slide"
]

# Combine the daily and weekly runtimes
timing_dt_combined <- rbindlist(
    list(runtimes_daily_dt,
         runtimes_weekly_dt,
         runtimes_special_dt
    ),
    fill = TRUE
)

# Remove slide
timing_dt_combined[, slide := NULL]

# Round times
timing_dt_combined[, timing := round(lubridate::as.duration(timing), 1)]

# reshape the data to wide
timing_dt_reshaped <- timing_dt_combined |>
    dcast(date ~ type, value.var = "timing")

# Add categorical indicator of comparison
# timing_dt_reshaped[, relative_speed := ifelse(daily > weekly, "Daily_worse", "Weekly_worse")]
# 
# # Add missing dates from case data for alignment
# timing_dt_complete <- merge(
#     complete_dates_dt,     
#     timing_dt_reshaped, 
#     by = "date",  
#     all.y = FALSE, 
#     all.x = TRUE 
# )

# @james TODO: not sure how to think about this w/ addition of rescaled forecast

## Runtimes plot
# runtimes_plt <- ggplot() +
# 	geom_point(data = timing_dt_combined,
# 	    aes(x = date,
# 	        y = timing,
# 	        shape = type
# 	    )
# 	) +
#     geom_linerange(
#         data = timing_dt_complete,
#         aes(x = date,
#             ymin = daily,
#             ymax = weekly,
#             color = relative_speed
#         )
#     ) +
#     # scale_y_continuous(sec.axis = sec_axis(~ . + 10000)) +
#     scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
#     scale_color_brewer(na.translate = FALSE, palette = "Accent") +
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#     labs(shape = "forecast", y = "run time (secs)")
# 
# runtimes_plt

### Diagnostics
# Add the dates by slide
diagnostics_dt <- diagnostics_dt[
    slide_dates_dictionary,
    on = "slide"
]

# Add missing dates from case data for date alignment in panel plot
diagnostics_dt_complete <- merge(
    complete_dates_dt,     
    diagnostics_dt, 
    by = "date",  
    all.y = FALSE, 
    all.x = TRUE 
)

# Reshape the ESS per sec columns into a single column for plotting
diagnostics_dt_long <- melt(
    diagnostics_dt_complete,
    measure.vars = c("fit_ess_basic_ps", "fit_ess_bulk_ps", "fit_ess_tail_ps"),
    variable.name = "ess_type",
    value.name = "ess_value"
)

# Shorten ess_type values
diagnostics_dt_long[, ess_type := gsub("fit_ess_(.*)_ps", "\\1", ess_type)]

# Plot
diagnostics_plt <-
    ggplot(diagnostics_dt_long[ess_type == "tail"], # Only plotting ESS tail
           aes(x = date,
               y = ess_value,
               color = type
           )
    ) +
    geom_line(aes(color = type)) +
    geom_point(size = 1) +
    scale_y_log10() +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_color_brewer(na.translate = FALSE, palette = "Dark2") +
    labs(
        # title = "Effective sample size per second",
        x = "Date",
        y = "ESS tail per sec (log10)",
        color = "Forecast scale",
        linetype = "Data"
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    ) +
    theme_minimal()

diagnostics_plt

# Patchwork
panel_fig <- (cases_plt / score_plt / diagnostics_plt) +
    plot_annotation(title = paste(daily_cases$province[1])) &
    theme_minimal() &
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") &
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


ggsave(tail(.args, 1), panel_fig, bg = "white", width = 12, height = 6)
