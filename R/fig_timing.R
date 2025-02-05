library(data.table)
library(ggplot2)

.args <- if (interactive()) {
  c(
    file.path("local", "output", c("forecast_daily_EC.rds", "forecast_weekly_EC.rds")),
    file.path("local", "figures", "benchmarks_EC.png")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}

# Original data
# Forecasts
daily_dt <- readRDS(.args[1])$timing |> rbindlist()
weekly_dt <- readRDS(.args[2])$timing |> rbindlist()

# Create groupable data for the plots
daily_dt$type <- "daily"
weekly_dt$type <- "weekly"
timing_dt_combined <- rbind(daily_dt, weekly_dt)
timing_dt_combined <- timing_dt_combined[, timing := lubridate::as.duration(timing)]

forecast_timing <- ggplot(
  data = timing_dt_combined,
  aes(x = slide, y = timing, fill = type),
) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  labs(
    x = "slide (days)",
    y = "Run time (seconds)"
  ) +
  theme_minimal()

ggsave(tail(.args, 1), forecast_timing, bg = "white", width = 12, height = 6)
