# ---------------------------
##
## Script name: 2025_RF_indicators_aggregate
##
## Project: GPE_2025_RF
##
## Purpose of script: Aggregate all RF indicators
##
## Author: Andrei Wong Espejo
##
## Date Created: 2022-10-25
##
## Email: awongespejo@worldbank.org
##
## ---------------------------
##
## Notes: Aggregation of all indicators and create country/entity level database
##   
##
## ---------------------------

## Program Set-up ------------

options(scipen = 100, digits = 4) # Prefer non-scientific notation

## Load required packages ----

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(renv, conflicted, here)

#renv::init() # 1st run 2022-10-25
#renv::snapshot() # Only run if changes occur
renv::status()

#report::report(sessionInfo())
#renv::dependencies()

## Do the following --------
# 1. Download the 2025_RF folder to the main directory (see files) and unzip it.
# 2. Run all the code by pressing Ctrl + Alt + R

source(here("2025_RF_indicators.R"))
source(here("2025_RF_user_db.R"))
