## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Libraries and setup
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# install.packages("tidyverse")
library(tidyverse)
library(sf)
#library(ggtext) ## Not used
library(ggrepel)
library(ggspatial)
library(ggpubr)

## Set ggplot theme to theme_bw()
theme_set(theme_bw())

