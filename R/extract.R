
library(data.table)

.args <- if (interactive()) c(
  file.path("data", "intermediate.rds"),
  "daily", "GP",
  file.path("data", "daily_GP.rds")
) else commandArgs(trailingOnly = TRUE)

target_province <- .args[3]

dt <- readRDS(.args[1])[province == target_province]

mode <- match.arg(.args[2], c("daily", "weekly"))

if (mode == "weekly") {
  fill_dt <- dt[, .(province, date = as.IDate(min(date):max(date)))]
  dt[, cumulative_incidence := cumsum(nafill(daily_incidence, fill = 0))]
  dt[, day_of_week := wday(date)]
  dt <- merge(dt[
    day_of_week == 7
  ][,
    .(province, date, daily_incidence = c(cumulative_incidence[1], diff(cumulative_incidence)))
  ], fill_dt, by = c("province", "date"), all = TRUE)
}

dt |> saveRDS(tail(.args, 1))

