## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Load data
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## List files "data"
list.files("data")


plot <- read_csv("data/plot.csv")

subplot <- read_csv("data/subplot.csv")

tree <- read_csv("data/tree.csv")

dw <- read_csv("data/deadwood.csv")

stump <- read_csv("data/stump.csv")

species_list <- read_csv("data/species_list.csv")
