## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Libraries
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Requires Rtools.exe to be installed alongside R:
## https://cran.r-project.org/bin/windows/Rtools/rtools40.html

## Requires devtools to install the ona package
# install.packages("devtools")
# library(devtools)
# devtools::install_github("onaio/ona.R")

## Installing tidyverse for data analysis and stringi for complex characters 
# https://r4ds.had.co.nz/
# install.packages(c("tidyverse", "stringi"))

library(ona)
library(tidyverse)
library(stringi)

## Set ggplot theme to theme_bw()
theme_set(ggplot2theme_bw())

## Create Lao R profile for windows
## Create a txt files in File > New File > Text File
## Copy/Paste this command: Sys.setlocale("LC_CTYPE", "lao")
## Save as ".Rprofile" in the project directory
