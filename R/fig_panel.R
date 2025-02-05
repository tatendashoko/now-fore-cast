library(data.table)
library(ggplot2)
library(patchwork)

.args <- if (interactive()) c(
    file.path("local", "data", c("daily_EC.rds", "weekly_EC.rds")), # cases
    file.path("local", "output", "score_EC.rds"), # scores
    file.path("local", "output", c("forecast_daily_EC.rds", "forecast_weekly_EC.rds")), # forecasts (also contains timing)
    file.path("local", "output", "diagnostics_EC.csv"), # diagnostics
    file.path("local", "figures", "panel_fig_EC.png") # diagnostics
) else commandArgs(trailingOnly = TRUE)

# Load the raw data
# Cases
daily_cases <- readRDS(.args[1])
weekly_cases <- readRDS(.args[2])
# Scores
scores <- readRDS(.args[3])
# Get slide <-> date dictionary
forecasts_daily_dt <- readRDS(.args[4])$forecast |>
    rbindlist()

slide_dates_dictionary <- forecasts_weekly_dt[, .SD[1], by = "slide", .SDcols = c("date")]
# Diagnostics
diagnostics_dt <- fread(.args[6])

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
	        color = forecast,
	        linetype = data
	    )
	) +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_y_log10() +
    scale_linetype_manual(values = c("daily" = "solid", "weekly" = "dashed"),
                          breaks = c("daily", "weekly")) +
    scale_color_brewer(na.translate = FALSE, palette = "Dark2") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(y = "CRPS (log-scale)",
         linetype = "Data",
         color = "Forecast scale"
    )

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
    measure.vars = c("ess_basic_ps", "ess_bulk_ps", "ess_tail_ps"),
    variable.name = "ess_type",
    value.name = "ess_value"
)

# Plot
divergences_plt <- 
    ggplot(diagnostics_dt_long, 
           aes(x = date,
               y = ess_value,
               color = ess_type,
               linetype = type
           )
    ) +
    geom_line() +
    geom_point(size = 1) +
    scale_y_log10() +
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") +
    scale_color_manual(values = c("#1F77B4", "#FF7F0E", "#2CA02C")) +
    scale_linetype_manual(
        values = c("daily" = "solid", "weekly" = "dashed"),
        breaks = c("daily", "weekly")
    ) +
    labs(
        # title = "Effective sample size per second",
        x = "Date",
        y = "ESS per sec (log10)",
        color = "ESS type",
        linetype = "Data"
    ) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
    ) +
    theme_minimal()

# Patchwork
panel_fig <- (cases_plt / score_plt / divergences_plt) +
    plot_annotation(title = paste(daily_cases$province[1])) &
    theme_minimal() &
    scale_x_date(NULL, date_breaks = "month", date_labels = "%b '%y") &
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


ggsave(tail(.args, 1), panel_fig, bg = "white", width = 12, height = 6)
