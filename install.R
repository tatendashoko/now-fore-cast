
stopifnot("Must have `renv` installed." = require(renv))

renv::activate()
res <- renv::restore(prompt = FALSE)

print(res)

# if (!require(data.table)) {
# 	install.packages("data.table")
# }
# 
# if (!require(ggplot2)) {
# 	install.packages("ggplot2")
# }
# 
# if (!require(EpiNow2)) {
# 	install.packages("EpiNow2", repos = c("https://epiforecasts.r-universe.dev", getOption("repos")))
# }
# 
# if (!require(scoringutils)) {
# 	install.packages("scoringutils")
# }