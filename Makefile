
default: data/intermediate.rds

R = $(strip Rscript $^ $(1) $@)

data:
	mkdir -p $@

# TODO: wget this instead
data/raw.csv: | data
	mv ~/Downloads/covid19za_provincial_cumulative_timeline_confirmed.csv $@

data/intermediate.rds: R/import.R data/raw.csv
	$(call R)

# needs some tweaking, but basically right
figures/incidence.png: R/fig_incidence.R data/intermediate.rds
	$(call R)
