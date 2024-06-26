# Load pacman for package 
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Packages
pacman::p_load(here,
               rstan,
               tidyverse,
               janitor,
               EpiNow2,
               data.table,
               scoringutils,
               data.table
)

# Set Global option
options(mc.cores=4)