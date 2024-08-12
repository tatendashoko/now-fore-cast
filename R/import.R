
library(data.table)

.args <- if (interactive()) c(
  file.path("data", c(
    "raw.csv",
    "intermediate.rds"
  ))
) else commandArgs(trailingOnly = TRUE)

raw_dt <- fread(.args[1], drop = c("YYYYMMDD", "total", "source", "UNKNOWN"))
raw_dt[, date := as.IDate(date, "%d-%m-%Y")]
raw_dt <- raw_dt[!(date %in% as.Date(c("2020-03-27", "2020-04-07")))] # these dates are all NAs

if (raw_dt[, any(is.na(.SD)), .SDcols = -c("date")]) stop("NA data entries")

long_dt <- raw_dt |> melt.data.table(
	id.vars = "date", variable.name = "province", value.name = "cumulative_incidence"
)

fix_blips <- function(incidence) {
	negative_incidence <- which(incidence < 0)
	if (length(negative_incidence)) {
		# erroneous blips up: looks like +X, -X => push -cases back
		blipup <- incidence[negative_incidence - 1] == -incidence[negative_incidence]
		# erroneous blips down: looks like -X, +X => push -cases forward
		blipdown <- -incidence[negative_incidence] == incidence[negative_incidence + 1]
		stopifnot("ambiguous blips" = !any(blipup & blipdown))
		incidence[c(negative_incidence[blipdown], negative_incidence[blipdown]+1)] <- 0
		incidence[c(negative_incidence[blipup]-1, negative_incidence[blipup])] <- 0
	}
	incidence
}

fix_swaps <- function(incidence, cinc) {
	negative_incidence <- which(incidence < 0)
	if (length(negative_incidence)) {
		# where swaps would fix the problem: cinc looks like A, A+C, A+B, A+D, ... => swap A+C, A+B
		swaps <- negative_incidence[(cinc[negative_incidence-1] > cinc[negative_incidence]) & (cinc[negative_incidence - 1] < cinc[negative_incidence+1])]
		shift <- incidence[swaps]
		incidence[swaps - 1] <- incidence[swaps - 1] + shift
		incidence[swaps] <- -incidence[swaps]
		incidence[swaps + 1] <- incidence[swaps - 1] + shift
	}
	incidence
}

fix_mean <- function(incidence) {
	negative_incidence <- which(incidence < 0)
	if (length(negative_incidence)) {
		shift <- floor((incidence[negative_incidence] + incidence[negative_incidence + 1])/2)
		mns <- which(shift >= 0)
		incidence[negative_incidence[mns]+1] <- incidence[negative_incidence[mns]+1] + incidence[negative_incidence[mns]] - shift[mns]
		incidence[negative_incidence[mns]] <- shift[mns]
	}
	negative_incidence <- which(incidence < 0)
	if (length(negative_incidence)) {
		shift <- floor((incidence[negative_incidence-1] + incidence[negative_incidence])/2)
		mns <- which(shift >= 0)
		incidence[negative_incidence[mns]-1] <- incidence[negative_incidence[mns]-1] + incidence[negative_incidence[mns]] - shift[mns]
		incidence[negative_incidence[mns]] <- shift[mns]
	}
	incidence
}

long_dt[
  order(date), confirm := {
  	holder <- integer(length(cumulative_incidence))
  	holder[is.na(cumulative_incidence)] <- NA_integer_
  	holder[!is.na(cumulative_incidence)] <- c(cumulative_incidence[1], diff(cumulative_incidence[!is.na(cumulative_incidence)]))
  	holder |>
  	# flatten blips
  	fix_blips() |>
  	# fix swaps
		fix_swaps(cumulative_incidence) |>
  	fix_mean()
  },
  by = province
]

if (long_dt[, any(confirm < 0)]) stop("Negative incidence.")

complete_dt <- long_dt[, CJ(province = unique(province), date = as.IDate(min(date):max(date))) ]

merge(
  complete_dt, long_dt, by = c("province", "date"), all = TRUE
)[, .(province, date, confirm)] |> saveRDS(tail(.args, 1))

