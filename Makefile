
default: data/intermediate.rds

R = $(strip Rscript $^ $(1) $@)

data:
	mkdir -p $@

# TODO: wget this instead
data/raw.csv: | data
	mv ~/Downloads/covid19za_provincial_cumulative_timeline_confirmed.csv $@

data/intermediate.rds: R/import.R data/raw.csv
	$(call R)
