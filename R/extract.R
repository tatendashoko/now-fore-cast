
library(data.table)

.args <- if (interactive()) c(
  file.path("local", "data", "intermediate.rds"),
  "daily", "GP",
  file.path("local", "data", "daily_GP.rds")
) else commandArgs(trailingOnly = TRUE)

target_province <- .args[3]

dt <- readRDS(.args[1])[province == target_province]

weekly_target <- 6 # 1 == Sunday, 6 == Friday
# front of series must be extended, so that stride aligns with weekly target
start_day_of_week <- (weekly_target %% 7) + 1
series_start <- dt[, min(date) |> wday()]

# if the series doesn't start on the necessary stride
if (series_start != start_day_of_week) {
  # need to start earlier
  prepend_length <- series_start + (start_day_of_week %% 7)
  dt <- rbind(
    dt[1, .(province, date = date - prepend_length:1, confirm = NA_integer_)],
    dt
  )
}

mode <- match.arg(.args[2], c("daily", "weekly"))

if (mode == "weekly") {
  fill_dt <- dt[, .(province, date = as.IDate(min(date):max(date)))]
  dt[, cumulative_incidence := cumsum(nafill(confirm, fill = 0))]
  dt[, day_of_week := wday(date)]
  dt <- merge(dt[
    day_of_week == weekly_target
  ][,
    .(province, date, confirm = c(cumulative_incidence[1], diff(cumulative_incidence)))
  ], fill_dt, by = c("province", "date"), all = TRUE)
}

dt |> saveRDS(tail(.args, 1))

