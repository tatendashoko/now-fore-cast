
library(data.table)

.args <- if (interactive()) c(
  file.path("data", c(
    "raw.csv",
    "intermediate.rds"
  ))
) else commandArgs(trailingOnly = TRUE)

raw_dt <- fread(.args[1], drop = c("YYYYMMDD", "total", "source"))
raw_dt[, date := as.IDate(date, "%d-%m-%Y")]

# correct_cumulative_cases <- function(cumulative_cases) {
#   corrected_cases <- cumulative_cases
#   n <- length(corrected_cases)
#   for (i in 2:(n-1)) {
#     if ((corrected_cases[i - 1] > corrected_cases[i + 1]) & (corrected_cases[i] > corrected_cases[i - 1])) {
#       corrected_cases[i + 1] <- corrected_cases[i]
#     } else if ((corrected_cases[i] < corrected_cases[i - 1]) & (corrected_cases[i] < corrected_cases[i + 1])) {
#       corrected_cases[i] <- corrected_cases[i - 1]
#     } else if ((corrected_cases[i] > corrected_cases[i - 1]) & (corrected_cases[i] > corrected_cases[i + 1])) {
#       corrected_cases[i] <- corrected_cases[i - 1]
#     }
#   }
#   # Handle the last element separately if needed
#   if (n > 1 && corrected_cases[n] < corrected_cases[n - 1]) {
#     corrected_cases[n] <- corrected_cases[n - 1]
#   }
#   return(corrected_cases)
# }

long_dt <- raw_dt |> melt.data.table(id.vars = "date", variable.name = "province", value.name = "cumulative_incidence")

long_dt[
  order(date),
  confirm := c(cumulative_incidence[1], diff(cumulative_incidence)),
  by = province
]

complete_dt <- long_dt[, CJ(province = unique(province), date = as.IDate(min(date):max(date))) ]

merge(
  complete_dt, long_dt, by = c("province", "date"), all = TRUE
)[, .(province, date, confirm)] |> saveRDS(tail(.args, 1))

