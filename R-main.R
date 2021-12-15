## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Main script for global setup and sourcing smaller scripts
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Setup --------------------------------------------------------------------

## Making NFI standardized tables from raw data -----------------------------
# source("data-raw/create-data.R")

## Sourcing data analysis scripts -------------------------------------------
source("R/00-libraries.R")

source("R/00-functions.R")

source("R/01-load-data.R")

source("R/02-data-checks.R")

source("R/03-tree-agb.R")


## Deadwood AGB

## Stump AGB

source("R/06-subplot-cstock.R")

source("R/07-plot-cstock.R")

source("R/08-forest-cstock.R")

## Reporting tables and figures

## Reporting maps

