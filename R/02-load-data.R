## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Load data
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Load province codes
province <- read_csv("data-raw/province.csv")

## Load subplot data from 2018 NFI3 file
subplot_init <- read_csv("data-raw/NFI3_2018_Inventory_v1.csv")

subplot <- subplot_init %>%
  select(
    -starts_with("tree_"), -starts_with("access"), 
    -starts_with("plot_info2"), -starts_with("ntfp")
    ) %>%
  rename_with(.fn = ~ str_remove(string = ., pattern = ".*/")) %>%
  #rename_with(.fn = ~ str_remove(string = ., pattern = "_nest[0-9]")) %>%
  rename_with(.fn = ~ str_sub(., 1 + (stringi::stri_sub(., 1, 1) == "_"))) #%>%
  select(
    subplot_no = index, plot_id = plot_code_nmbr, plot_type, subplot_id = sub_plot, 
    GPS_latitude, GPS_longitude, lc_type, lc_class, avg_height, can_cov
  ) %>%
  left_join(province, by = "plot_id")

table(subplot$plot_type)

## Load tree data fronm nested CSV
tree_files <- list.files(path = "data-raw", pattern = "tree_data", full.names = T)

tree_list <- map(tree_files, read_csv)

names(tree_list) <- tree_files %>% str_remove(".*_data_") %>% str_remove(".csv")


## Simplify column names 
tree_list <- map(.x = tree_list, .f = function(x){
  
  x %>%
    rename_with(.fn = ~ str_remove(string = ., pattern = ".*/")) %>%
    rename_with(.fn = ~ str_remove(string = ., pattern = "_nest[0-9]")) %>%
    rename_with(.fn = ~ str_sub(., 1 + (stringi::stri_sub(., 1, 1) == "_")))
  
})


## Combine tree data into one table
data_raw <- bind_rows(tree_list, .id = "file")

unique(data_raw$parent_table_name)
unique(data_raw$parent_index)

## Make live tree table
tree <- data_raw %>%
  filter(t_deadcl == 0 | is.na(t_deadcl)) %>%
  mutate(
    tree_no = paste0(file, "_tree", calc_digits(index), index),
    tree_id = paste0(file, "_sp", calc_digits(parent_index), parent_index, "_t", calc_digits(index), index)
  ) %>%
  select(file, subplot_no = parent_index, tree_id, tree_no, tree_dbh = t_dbh, tree_species = t_species_name)

## Check duplicates
tt <- tree %>% select(-file, -tree_id, -tree_no) %>% distinct()

## Make deadwood table
dw <- data_raw %>%
  filter(t_deadcl %in% c(1,2)) %>%
  mutate(
    dw_no   = paste0(file, "_tree", calc_digits(index), index),
    dw_id   = paste0(file, "_sp", calc_digits(parent_index), parent_index, "_t", calc_digits(index), index),
    dw_class     = if_else(is.na(t_deadcl), 0, t_deadcl),
    dw_tallshort = if_else(is.na(t_deadcl2_tallshort), 0, t_deadcl2_tallshort), 
    dw_code = dw_class * 10 + dw_tallshort,
    dw_dbh = case_when(
        dw_code == 10 &  is.na(t_dbh) ~ t_dead_dbh, ##  Inconsistent dead_dbh in nest3
        dw_code == 10 & !is.na(t_dbh) ~ t_dbh,
        dw_code == 21                 ~ t_dead_DBH_short,
        dw_code == 22                 ~ t_dead_DBH_tall,
        TRUE ~ NA_real_
        ),
    dw_height = case_when(
        dw_code == 21 ~ t_dead_height_short,
        dw_code == 22 ~ t_dead_dist_t_tall * (tan(t_dead_slope_t_tall * pi / 180) + tan(t_dead_slope_b_tall * pi / 180)),
        TRUE ~ NA_real_
        ),
    dw_hmax = if_else(dw_code == 22, 1.3 /(1 - dw_dbh / t_dead_DB_tall), NA_real_)
  ) %>%
  select(file, subplot_no = parent_index, dw_id, dw_no, dw_class, dw_code, dw_dbh, dw_height, dw_hmax, dw_species = t_species_name)
dw

## Make stump table
stump <- data_raw %>%
  filter(t_deadcl == 3) %>%
  mutate(
    stump_no = paste0(file, "_tree", calc_digits(index), index),
    stump_id = paste0(file, "_sp", calc_digits(parent_index), parent_index, "_t", calc_digits(index), index),
    stump_diameter = (diameter1 + diameter2)/2,
    stump_height = height_st / 100
  ) %>%
  select(file, subplot_no = parent_index, stump_id, stump_no, stump_diameter, stump_height, stump_species = t_species_name)
stump




