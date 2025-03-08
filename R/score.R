
library(data.table)
library(scoringutils)

.args <- if (interactive()) c(
  file.path("local", "data", c("daily_GP.rds", "weekly_GP.rds")),
  file.path("local", "output", c("forecast_daily_GP.rds", "forecast_weekly_GP.rds", "forecast_special_GP.rds")),
  file.path("local", "output", "score_GP.rds")
) else commandArgs(trailingOnly = TRUE)

# True data
daily_ref_dt <- readRDS(.args[1]) |> setnames("confirm", "true_value")
weekly_ref_dt <- readRDS(.args[2]) |> setnames("confirm", "true_value")
# Forecasts
daily_fore_dt <- readRDS(.args[3])$forecast |> rbindlist() |> setnames("value", "prediction")
weekly_fore_dt <- readRDS(.args[4])$forecast |> rbindlist() |> setnames("value", "prediction")
special_fore_dt <- readRDS(.args[5])$forecast |> rbindlist() |> setnames("value", "prediction")

score_dt <- rbind(
# daily forecast vs daily data
(daily_fore_dt[
  daily_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
] |> score())[,
  .(date = min(date), crps = mean(crps), forecast = "daily", data = "daily"),
  by = slide
],

# daily forecasts vs weekly data
(daily_fore_dt[
  weekly_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
][, csum := cumsum(prediction), by = .(sample, slide) ][!is.na(true_value),
  .(date, prediction = c(csum[1], diff(csum)), true_value), by = .(sample, slide)
] |> score())[,
  .(date = min(date), crps = mean(crps), forecast = "daily", data = "weekly"),
  by = slide
],

# weekly forecasts vs daily data
(weekly_fore_dt[
  daily_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
] |> score())[,
  .(date = min(date), crps = mean(crps), forecast = "weekly", data = "daily"),
  by = slide
],

# weekly forecasts vs weekly data
(weekly_fore_dt[
  weekly_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
][, csum := cumsum(prediction), by = .(sample, slide) ][!is.na(true_value),
                                                        .(date, prediction = c(csum[1], diff(csum)), true_value), by = .(sample, slide)
] |> score())[,
              .(date = min(date), crps = mean(crps), forecast = "weekly", data = "weekly"),
              by = slide
],

# weekly-scale forecasts vs weekly data
(special_fore_dt[
  weekly_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
] |> score())[,
              .(date = min(date), crps = mean(crps), forecast = "rescale", data = "weekly"),
              by = slide
]

)

saveRDS(score_dt, tail(.args, 1))