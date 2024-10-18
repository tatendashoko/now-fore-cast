
library(data.table)
library(scoringutils)

.args <- if (interactive()) c(
  file.path("local", "data", c("daily_GP.rds", "weekly_GP.rds")),
  file.path("local", "output", c("forecast_daily_GP.rds", "forecast_weekly_GP.rds")),
  file.path("local", "output", "score_GP.rds")
) else commandArgs(trailingOnly = TRUE)

daily_ref_dt <- readRDS(.args[1]) |> setnames("confirm", "true_value")
weekly_ref_dt <- readRDS(.args[2]) |> setnames("confirm", "true_value")
daily_fore_dt <- readRDS(.args[3])$forecast |> rbindlist() |> setnames("value", "prediction")
weekly_fore_dt <- readRDS(.args[4])$forecast |> rbindlist() |> setnames("value", "prediction")

score_dt <- rbind(
# daily => daily
(daily_fore_dt[
  daily_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
] |> score())[,
  .(date = min(date), crps = mean(crps), from = "daily", to = "daily"),
  by = slide
],

# daily => weekly
(daily_fore_dt[
  weekly_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
][, csum := cumsum(prediction), by = .(sample, slide) ][!is.na(true_value),
  .(date, prediction = c(csum[1], diff(csum)), true_value), by = .(sample, slide)
] |> score())[,
  .(date = min(date), crps = mean(crps), from = "daily", to = "weekly"),
  by = slide
],

# weekly => daily
(weekly_fore_dt[
  daily_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
] |> score())[,
  .(date = min(date), crps = mean(crps), from = "weekly", to = "daily"),
  by = slide
],

# weekly => weekly
(weekly_fore_dt[
  weekly_ref_dt, on = .(date),
  .(sample, slide, date, prediction, true_value),
  nomatch = 0
][, csum := cumsum(prediction), by = .(sample, slide) ][!is.na(true_value),
                                                        .(date, prediction = c(csum[1], diff(csum)), true_value), by = .(sample, slide)
] |> score())[,
              .(date = min(date), crps = mean(crps), from = "weekly", to = "weekly"),
              by = slide
]

)

saveRDS(score_dt, tail(.args, 1))