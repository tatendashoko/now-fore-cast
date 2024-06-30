
default: data/intermediate.rds

R = $(strip Rscript $^ $(1) $@)

data:
	mkdir -p $@

figures:
	mkdir -p $@

# TODO: wget this instead
data/raw.csv: | data
	mv ~/Downloads/covid19za_provincial_cumulative_timeline_confirmed.csv $@

data/intermediate.rds: R/import.R data/raw.csv
	$(call R)

# TODO probably use file name special variable + $(subst _, ,XXX) to consolidate these
data/daily_%.rds: R/extract.R data/intermediate.rds
	$(call R,daily $*)

data/weekly_%.rds: R/extract.R data/intermediate.rds
	$(call R,weekly $*)

PROVINCES := EC FS GP KZN LP MP NC NW WC

alldaily: $(patsubst %,data/daily_%.rds,${PROVINCES})

allweekly: $(patsubst %,data/weekly_%.rds,${PROVINCES})

# needs some tweaking, but basically right
figures/incidence.png: R/fig_incidence.R data/intermediate.rds | figures
	$(call R)

figures/daily_vs_weekly_%.png: R/fig_daily_vs_weekly.R data/daily_%.rds data/weekly_%.rds | figures
	$(call R)

alldvswfigs: $(patsubst %,figures/daily_vs_weekly_%.png,${PROVINCES})