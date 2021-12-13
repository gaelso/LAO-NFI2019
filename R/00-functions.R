## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Custom functions
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Simplify column names by removing repetitive words (nest or t_). 
rename_cols <- function(.tab){
  names(.tab) <- names(.tab) %>% 
    str_replace_all("nest1|nest2|nest3|nest4", "nest") %>%
    str_remove(".*/") %>%
    str_remove("_nest") %>%
    str_replace_all("t_dead_Db_tall", "t_dead_DB_tall")  %>% ## Inconsistent column name between nest1-2 and 3-4
    str_sub(., 1 + 2*(stri_sub(., 1, 2) == "t_")) %>% ## remove "t_" as first 2 letters
    str_sub(., 1 + (stri_sub(., 1, 1) == "_")) ## remove "_" as first letter
  
  return(.tab)
}

## Add zeros to numbers changed to characters to have them sorted correctly.
calc_digits <- function(.var){
  str_sub(10^(trunc(log10(max(.var))) - trunc(log10(.var))), start = 2)
}

