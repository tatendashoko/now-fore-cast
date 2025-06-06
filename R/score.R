
library(data.table)
library(scoringutils)

.args <- if (interactive()) {
	.prov <- "GP"
	c(
	  sprintf(file.path("local", "data", "%s_%s.rds"), c("daily", "weekly"), .prov),
	  sprintf(file.path("local", "output", "forecast_%s_%s.rds"), c("daily", "weekly", "special"), .prov),
	  sprintf(file.path("local", "output", "score_%s.rds"), .prov)
	)
} else commandArgs(trailingOnly = TRUE)

# True data
daily_ref_dt <- readRDS(.args[1]) |> setnames("confirm", "true_value")
weekly_ref_dt <- readRDS(.args[2]) |> setnames("confirm", "true_value")
# Forecasts
daily_fore_dt <- readRDS(.args[3])$forecast |> rbindlist() |> setnames("value", "prediction")
weekly_fore_dt <- readRDS(.args[4])$forecast |> rbindlist() |> setnames("value", "prediction")
special_fore_dt <- readRDS(.args[5])$forecast |> rbindlist() |> setnames("value", "prediction")

dtextract <- function(dt, fct, dat) dt[,
  .(date, crps, forecast = fct, data = dat),
	by = slide
]

join_and_score <- function(fore_dt, ref_dt) fore_dt[
	ref_dt, on = .(date), .(sample, slide, date, prediction, true_value),
	nomatch = 0
][,
	csum := cumsum(prediction), by = .(sample, slide)
][!is.na(true_value),
	.(date, prediction = c(csum[1], diff(csum)), true_value),
	by = .(sample, slide)
][,
  `:=`(predicted = prediction, observed = true_value)
] |>
    as_forecast_sample(
        predicted = "prediction",
        observed = "true_value",
        sample_id = "sample"
    ) |>
    score()

# wherever the true value is NA, we are assuming the prediction should be
# accumulated to wherever the next observation occurs

score_dt <- rbind(
# daily forecast vs daily data
join_and_score(daily_fore_dt, daily_ref_dt) |> dtextract("daily", "daily"),

# daily forecasts vs weekly data
join_and_score(daily_fore_dt, weekly_ref_dt) |> dtextract("daily", "weekly"),

# weekly forecasts vs daily data
join_and_score(weekly_fore_dt, daily_ref_dt) |> dtextract("weekly", "daily"),

# weekly forecasts vs weekly data
join_and_score(weekly_fore_dt, weekly_ref_dt) |> dtextract("weekly", "weekly"),

# weekly-scale forecasts vs weekly data
join_and_score(special_fore_dt, weekly_ref_dt) |> dtextract("rescale", "weekly")

)

saveRDS(score_dt, tail(.args, 1))