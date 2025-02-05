
library(data.table)

.args <- if (interactive()) c(
	list.files("local", "data", "daily", full.names = TRUE) |> grep("RSA", x = _, value = TRUE, invert = TRUE),
	file.path("local", "data", "daily_RSA.rds")
) else commandArgs(trailingOnly = TRUE)

all_dt <- (head(.args, -1) |> lapply(readRDS) |> rbindlist())

if (all_dt[,any(is.na(confirm)) & !all(is.na(confirm)), by=date][,any(V1)]) stop("mixed NAs by province.")

all_dt[,.(province = "RSA", confirm = sum(confirm)), keyby=date] |> saveRDS(tail(.args, 1))
