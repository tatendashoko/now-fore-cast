library(data.table)
library(ggplot2)
library(patchwork)

.args <- if (interactive()) c(
    file.path("local", "data", c("daily_WC.rds", "weekly_WC.rds")), # cases
    file.path("local", "output", "score_WC.rds"), # scores
    file.path("local", "output", c("forecast_daily_WC.rds", "forecast_weekly_WC.rds")), # forecasts (also contains timing)
    file.path("local", "output", "diagnostics_WC.csv"), # diagnostics
    file.path("local", "figures", "panel_fig_WC.png") # diagnostics
) else commandArgs(trailingOnly = TRUE)

# Load the raw data
# Cases
daily_cases <- readRDS(.args[1])
weekly_cases <- readRDS(.args[2])
# Scores
scores <- readRDS(.args[3])
# Runtimes
# Daily
runtimes_daily_dt <- readRDS(.args[4])$timing |>
    rbindlist()
runtimes_daily_dt[, type := "daily"]
# Weekly
runtimes_weekly_dt <- readRDS(.args[5])$timing |>
    rbindlist()
runtimes_weekly_dt[, type := "weekly"]
# Diagnostics
diagnostics_dt <- fread(.args[6])

# other data
train_window <- 70

#####
#Plots
####

# Cases
# The cases data has the complete dates, we'll need to complete obtained data (scores, diagnostics, and timing) when making the panel plot
complete_dates_dt <- daily_cases[, .(date)]

# cases plot
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
	scale_y_continuous(
		"Daily Incidence", sec.axis = sec_axis(
			~ . * 7, name = "Weekly Incidence" 
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
	        color = from,
	        linetype = to
	    )
	) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_y_log10() +
    scale_linetype_discrete(na.translate = FALSE) +
    scale_color_brewer(na.translate = FALSE, palette = "Set1") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(linetype = "data", color = "forecast")

score_plt

# Run times
# Daily data
# Add dates by slide
dates_by_slide <- scores[from == "daily" & to == "daily"][, .(slide, date)]

runtimes_daily_dt <- runtimes_daily_dt[
    dates_by_slide,
    on = "slide"
]

# Weekly data
runtimes_weekly_dt <- readRDS(.args[5])$timing |>
    rbindlist()

runtimes_weekly_dt[, type := "weekly"]

# Add dates by slide
runtimes_weekly_dt <- runtimes_weekly_dt[
    dates_by_slide,
    on = "slide"
]

# Combine the daily and weekly runtimes
timing_dt_combined <- rbind(runtimes_daily_dt, runtimes_weekly_dt)

# Remove slide
timing_dt_combined[, slide := NULL]

# Round times
timing_dt_combined[, timing := round(lubridate::as.duration(timing), 1)]

# reshape the data to wide
timing_dt_reshaped <- timing_dt_combined |>
    dcast(date ~ type, value.var = "timing")

# Add categorical indicator of comparison
timing_dt_reshaped[, relative_speed := ifelse(daily > weekly, "Daily_worse", "Weekly_worse")]

# Add missing dates from case data for alignment
timing_dt_complete <- merge(
    complete_dates_dt,     
    timing_dt_reshaped, 
    by = "date",  
    all.y = FALSE, 
    all.x = TRUE 
)
    
## Runtimes plot
runtimes_plt <- ggplot() +
	geom_point(data = timing_dt_combined,
	    aes(x = date,
	        y = timing,
	        shape = type
	    )
	) +
    geom_linerange(
        data = timing_dt_complete,
        aes(x = date,
            ymin = daily,
            ymax = weekly,
            color = relative_speed
        )
    ) +
    # scale_y_continuous(sec.axis = sec_axis(~ . + 10000)) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_color_brewer(na.translate = FALSE, palette = "Accent") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(shape = "forecast", y = "run time (secs)")

runtimes_plt

### Diagnostics
# Add the dates by slide
diagnostics_dt <- diagnostics_dt[
    dates_by_slide,
    on = "slide"
]

# Add missing dates from case data for alignment
diagnostics_dt_complete <- merge(
    complete_dates_dt,     
    diagnostics_dt, 
    by = "date",  
    all.y = FALSE, 
    all.x = TRUE 
)

# Plot
divergences_plt <- ggplot() +
    geom_line(data = diagnostics_dt_complete,
              aes(x = date,
                  y = divergent_transitions,
                  color = type
              )
    ) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_color_brewer(na.translate = FALSE, palette = "Set1") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(color = "forecast")

divergences_plt

# Patchwork
panel_fig <- (cases_plt / score_plt / runtimes_plt / divergences_plt) +
    plot_annotation(title = paste(daily_cases$province[1])) &
    theme_minimal() &
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") &
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


ggsave(tail(.args, 1), panel_fig, bg = "white", width = 12, height = 6)
