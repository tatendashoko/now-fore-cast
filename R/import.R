
library(data.table)

.args <- if (interactive()) c(
  file.path("data", c(
    "raw.csv",
    "intermediate.rds"
  ))
) else commandArgs(trailingOnly = TRUE)

raw_dt <- fread(.args[1], drop = c("YYYYMMDD", "total", "source"))
raw_dt[, date := as.IDate(date, "%d-%m-%Y")]

long_dt <- raw_dt |> melt.data.table(id.vars = "date", variable.name = "province", value.name = "cumulative_incidence")

long_dt[
  order(date),
  daily_incidence := c(cumulative_incidence[1], diff(cumulative_incidence)),
  by = province
]

complete_dt <- long_dt[, CJ(province = unique(province), date = as.IDate(min(date):max(date))) ]

merge(
  complete_dt, long_dt, by = c("province", "date"), all = TRUE
)[, .(province, date, daily_incidence)] |> saveRDS(tail(.args, 1))

