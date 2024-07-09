
library(data.table)

.args <- if (interactive()) c(
  file.path("data", c(
    "raw.csv",
    "intermediate.rds"
  ))
) else commandArgs(trailingOnly = TRUE)

raw_dt <- fread(.args[1], drop = c("YYYYMMDD", "total", "source", "UNKNOWN"))
raw_dt[, date := as.IDate(date, "%d-%m-%Y")]
raw_dt <- raw_dt[!(date %in% c("2020-03-27", "2020-04-07"))] # these dates are all NAs

if (raw_dt[, any(is.na(.SD)), .SDcols = -c("date")]) stop("NA data entries")

long_dt <- raw_dt |> melt.data.table(
	id.vars = "date", variable.name = "province", value.name = "cumulative_incidence"
)

fix_blips <- function(incidence) {
	errors <- which(incidence < 0)
	if (length(errors)) {
		# erroneous blips up: looks like +X, -X => push -cases back
		blipup <- incidence[errors - 1] == -incidence[errors]
		# erroneous blips down: looks like -X, +X => push -cases forward
		blipdown <- -incidence[errors] == incidence[errors + 1]
		stopifnot("ambiguous blips" = !any(blipup & blipdown))
		incidence[c(errors[blipdown], errors[blipdown]+1)] <- 0
		incidence[c(errors[blipup]-1, errors[blipup])] <- 0
	}
	incidence
}

fix_swaps <- function(incidence, cinc) {
	errors <- which(incidence < 0)
	if (length(errors)) {
		# where swaps would fix the problem: cinc looks like A, A+C, A+B, A+D, ... => swap A+C, A+B
		swaps <- errors[(cinc[errors-1] > cinc[errors]) & (cinc[errors - 1] < cinc[errors+1])]
		shift <- incidence[swaps]
		incidence[swaps - 1] <- incidence[swaps - 1] + shift
		incidence[swaps] <- -incidence[swaps]
		incidence[swaps + 1] <- incidence[swaps - 1] + shift
	}
	incidence
}

fix_mean <- function(incidence) {
	errors <- which(incidence < 0)
	if (length(errors)) {
		shift <- floor((incidence[errors] + incidence[errors + 1])/2)
		mns <- which(shift >= 0)
		incidence[errors[mns]+1] <- incidence[errors[mns]+1] + incidence[errors[mns]] - shift[mns]
		incidence[errors[mns]] <- shift[mns]
	}
	errors <- which(incidence < 0)
	if (length(errors)) {
		shift <- floor((incidence[errors-1] + incidence[errors])/2)
		mns <- which(shift >= 0)
		incidence[errors[mns]-1] <- incidence[errors[mns]-1] + incidence[errors[mns]] - shift[mns]
		incidence[errors[mns]] <- shift[mns]
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

