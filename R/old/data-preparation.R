
###
### Setup ###################################################################
###

## Libraries 
library(tidyverse)
library(stringi)

## Custom function to rename columns: remove nesting number and paths
rename_cols <- function(.tab){
  names(.tab) <- names(.tab) %>% 
    str_replace_all("nest1|nest2|nest3|nest4", "nest") %>%
    str_remove(".*/") %>%
    str_remove("_nest") %>%
    str_replace_all("t_dead_Db_tall", "t_dead_DB_tall")  %>% ## Inconsistent column name between nest1-2 adn 3-4
    str_sub(., 1 + 2*(stri_sub(., 1, 2) == "t_")) %>% ## remove "t_" as first 2 letters
    str_sub(., 1 + (stri_sub(., 1, 1) == "_")) ## remove "_" as first letter
  
  return(.tab)
}

calc_digits <- function(.var){
  str_sub(10^(trunc(log10(max(.var))) - trunc(log10(.var))), start = 2)
}



###
### Load data ###############################################################
### 

## --- Province data --------------------------------------------------------
province <- read_csv("data/original/province.csv")


## --- Plot data ------------------------------------------------------------
subplot_init <- read_csv("data/original/NFI3_2018_Inventory_v1.csv")

names(subplot_init) <- names(subplot_init) %>% 
  str_remove(".*/") %>%
  str_sub(., 1 + (stri_sub(., 1, 1) == "_")) ## remove "_" as first letter

subplot_all <- subplot_init %>%
  select(
    subplot_no = index, plot_id = plot_code_nmbr, plot_type, subplot_id = sub_plot, 
    GPS_latitude, GPS_longitude, lc_type, lc_class, avg_height, can_cov
    ) %>%
  left_join(province)

summary(subplot_all)

subplot_all %>%
  filter(sub_plot == "sub_plotA") %>%
  ggplot(aes(x = GPS_longitude, y = GPS_latitude, color = lc_class)) +
  geom_point() +
  coord_fixed()

subplot_all %>%
  filter(sub_plot == "sub_plotA") %>%
  ggplot(aes(x = GPS_longitude, y = GPS_latitude, color = province_name)) +
  geom_point() +
  coord_fixed()

## --- tree data ------------------------------------------------------------

## List files in data folder
files <- list.files(path = "data/original", pattern = "tree", full.names = TRUE)

## Read in block into a list
tree_init <- map(.x = files, .f = read_csv)

## Name list elements with file name
names(tree_init) <- files %>% str_remove(pattern = ".*_data_") %>% str_remove(".csv")

## Rename the table columns and combine file based tables into one data frame
tree_df <- map(.x = tree_init, .f = rename_cols) %>%
  bind_rows(.id = "file")

tree_df



###
### Data preparation ########################################################
###

## --- Harmonize variables --------------------------------------------------

## First_check
summary(tree_df)
table(tree_df$livedead, useNA = 'always')
table(tree_df$deadcl, useNA = 'always')
table(tree_df$deadcl, tree_df$deadcl2_tallshort, useNA = 'always')


## Harmonize dbh and h, simplify tree class
tree_cor <- tree_df %>%
  mutate(
    tree_id = paste0(file, "_tree", calc_digits(index), index),
    livedead = if_else(is.na(deadcl), 1, 2),
    deadcl = if_else(is.na(deadcl), 0, deadcl),
    deadcl2_tallshort = if_else(is.na(deadcl2_tallshort), 0, deadcl2_tallshort), 
    tree_dbh = dbh,
    stump_dbh = (diameter1 + diameter2)/2,
    class = case_when(
      deadcl == 0                          ~ "live tree",
      deadcl == 1                          ~ "dead tree cl1",
      deadcl == 2 & deadcl2_tallshort == 1 ~ "dead stem short", 
      deadcl == 2 & deadcl2_tallshort == 2 ~ "dead stem tall", 
      deadcl == 3                          ~ "stump", 
      TRUE ~ NA_character_
    ),
    class_code = deadcl * 10 + deadcl2_tallshort,
    dbh = case_when(
      class_code ==  0                    ~ tree_dbh,
      class_code == 10 &  is.na(tree_dbh) ~ dead_dbh, ##  Inconsistent dead_dbh in nest3
      class_code == 10 & !is.na(tree_dbh) ~ tree_dbh,
      class_code == 21                    ~ dead_DBH_short,
      class_code == 22                    ~ dead_DBH_tall,
      class_code == 30                    ~ stump_dbh,
      TRUE ~ NA_real_
    ),
    h = case_when(
      class_code == 21 ~ dead_height_short,
      class_code == 22 ~ dead_dist_t_tall * (tan(dead_slope_t_tall * pi / 180) + tan(dead_slope_b_tall * pi / 180)),
      class_code == 30 ~ height_st / 100,
      TRUE ~ NA_real_
    ),
    h_max = if_else(class_code == 22, 1.3 /(1 - dbh / dead_DB_tall), NA_real_)
  )

## Checks
head(tree_cor$tree_id)
table(tree_cor$class, tree_cor$class_code, useNA = "always")
summary(tree_cor$dbh)

tree_cor %>% filter(!is.na(h)) %>%
  ggplot(aes(x = dbh, y = h, color = class)) + 
  geom_point()


## --- Add scale factor and plot info ---------------------------------------
tree_all <- tree_cor %>%
  mutate(
    nest_radius = case_when(
      livedead == 1 & dbh <  30 ~  6,
      livedead == 1 & dbh <  50 ~ 15,
      livedead == 1 & dbh >= 50 ~ 20,
      livedead == 2 & dbh <  30 ~  6,
      livedead == 2 & dbh >= 30 ~ 25,
      TRUE ~ NA_real_
    ),
    scale_factor = 10000 / (nest_radius^2 * pi)
  ) %>%
  select(
    file, tree_id, species_name, class, class_code, dbh, h, h_max, dead_DB_short,
    dead_DT_short, dead_DB_tall, dead_dist_t_tall, dead_slope_b_tall, dead_slope_t_tall, nest_radius, 
    scale_factor, subplot_no = parent_index
  )

## Checks
table(tree_all$class_code, tree_all$nest_radius, useNA = "always")


## --- write tables ---------------------------------------------------------
write_csv(subplot_all, "data/subplot_all.csv")
write_csv(tree_all, "data/tree_all.csv")


## --- Prepare plot demo for Khammouan --------------------------------------
unique(subplot_all$province_name)

subplot_kha <- subplot_all %>% filter(province_name == "Khammouan")

subplot_list <- subplot_kha %>% pull(subplot_no) %>% unique() 

tree_kha <- tree_all %>% filter(subplot_no %in% subplot_list)

write_csv(subplot_kha, "data/subplot_kha.csv")
write_csv(tree_kha, "data/tree_kha.csv")





