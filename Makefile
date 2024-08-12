
# structural definitions
DATDIR ?= data
FIGDIR ?= figures
OUTDIR ?= output

default: allscores

# convenience definitions
R = $(strip Rscript $^ $(1) $@)

define md
$(1):
	mkdir -p $$@

endef

# define all the necessary directory creation
DIRS := ${DATDIR} ${FIGDIR} ${OUTDIR}

$(foreach dir,${DIRS},$(eval $(call md,${dir})))

.install_packages: R/install.R
	$(call R) & touch $@

DATAURL := https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv

# get the raw data
${DATDIR}/raw.csv: | ${DATDIR}
	wget -O $@ ${DATAURL}

# initial organization + saving as binary; no cleaning, only type conversion
# & pivoting to long
${DATDIR}/intermediate.rds: R/import.R ${DATDIR}/raw.csv | ${DATDIR} .install_packages
	$(call R)

# n.b. raw data also has an UNKNOWN
PROVINCES := EC FS GP KZN LP MP NC NW WC

# define all possible extracts
$(foreach agg,daily weekly,$(foreach tar,${PROVINCES},$(eval EXTRACTS += ${DATDIR}/${agg}_${tar}.rds)))

# extraction rule; also cleans data
${EXTRACTS}: R/extract.R ${DATDIR}/intermediate.rds
	$(call R,$(subst _, ,$(basename $(notdir $@))))

${DATDIR}/daily_RSA.rds: R/aggregate.R $(filter ${DATDIR}/daily_%.rds,${EXTRACTS})
	$(call R)

${DATDIR}/weekly_RSA.rds: R/aggregate.R $(filter ${DATDIR}/weekly_%.rds,${EXTRACTS})
	$(call R)

allextracts: ${EXTRACTS} ${DATDIR}/daily_RSA.rds ${DATDIR}/weekly_RSA.rds

# needs some tweaking, but basically right
${FIGDIR}/incidence.png: R/fig_incidence.R data/intermediate.rds | ${FIGDIR}
	$(call R)

${FIGDIR}/daily_vs_weekly_%.png: R/fig_daily_vs_weekly.R ${DATDIR}/daily_%.rds ${DATDIR}/weekly_%.rds | ${FIGDIR}
	$(call R)
	
${FIGDIR}/benchmarks_%.png: R/fig_timing.R ${OUTDIR}/forecast_daily_%.rds ${OUTDIR}/forecast_weekly_%.rds | ${FIGDIR}
	$(call R)

alldvswfigs: $(patsubst %,${FIGDIR}/daily_vs_weekly_%.png,${PROVINCES})

allbenchmarkfigs: $(patsubst %,${FIGDIR}/benchmarks_%.png,${PROVINCES})

${OUTDIR}/forecast_%.rds: R/pipeline.R data/%.rds | ${OUTDIR}
	$(call R)

${OUTDIR}/score_%.rds: R/score.R data/daily_%.rds data/weekly_%.rds ${OUTDIR}/forecast_daily_%.rds ${OUTDIR}/forecast_weekly_%.rds
	$(call R)

${OUTDIR}/diagnostics_%.csv: R/diagnostics.R output/forecast_daily_%.rds output/forecast_weekly_%.rds
	$(call R)

alldiagnostics: $(patsubst %,${OUTDIR}/diagnostics_%.csv,${PROVINCES})

allforecasts: $(patsubst %,${OUTDIR}/forecast_daily_%.rds,${PROVINCES} RSA) $(patsubst %,${OUTDIR}/forecast_weekly_%.rds,${PROVINCES} RSA)
allscores: $(patsubst %,${OUTDIR}/score_%.rds,${PROVINCES} RSA)