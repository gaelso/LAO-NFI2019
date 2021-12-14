## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## AGB for live trees
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Add scale factor, basal area and AGB for each tree
tree_agb <- tree %>%
  left_join(subplot %>% select(subplot_id, lc_class), by = "subplot_id") %>%
  filter(lc_class %in% c("EF", "MDF", "DD", "CF", "MCB")) %>%
  mutate(
    plot_radius = case_when(
        tree_dbh <= 10 ~  6,
        tree_dbh <= 30 ~ 15,
        tree_dbh <= 50 ~ 20,
        tree_dbh  > 50 ~ 25,  
        TRUE ~ NA_real_
        ),
    scale_factor = 10000 / (plot_radius^2 * pi),
    tree_ba = round((tree_dbh/200)^2 * pi, 2),
    tree_agb = case_when(
        lc_class == "EF"  ~ 0.3112   * tree_dbh^2.2331,
        lc_class == "DD"  ~ 0.2137   * tree_dbh^2.2575,
        lc_class == "MDF" ~ 0.523081 * tree_dbh^2     ,
        lc_class == "CF"  ~ 0.1277   * tree_dbh^2.3944,
        lc_class == "MCB" ~ 0.1277   * tree_dbh^2.3944,
        TRUE ~ NA_real_ 
        )
  )

tree_agb

## Checks
gr_treeagb <- tree_agb %>%
  filter(tree_dbh < 100) %>%
  ggplot(aes(x = tree_dbh, y = tree_agb, fill = lc_class)) +
  geom_point(shape = 21, size = 2) +
  scale_fill_viridis_d() 
gr_treeagb
