## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Data checks
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Plot level checks ########################################################

## --- Check plot main land cover class -------------------------------------

## Calculate number of subplot per plot and land cover class
subplot_lc <- subplot %>%
  filter(subplot_measured == "yes") %>%
  arrange(plot_id) %>%
  mutate(count = 1) %>%
  select(plot_id, subplot_no, lc_class, count) %>%
  distinct() %>%
  pivot_wider(names_from = subplot_no, values_from = count, values_fill = 0) %>%
  mutate(count = rowSums(x = .[3:12]))

plot_lc <- subplot_lc %>% 
  group_by(plot_id) %>%
  summarise(max_count = max(count)) %>%
  left_join(subplot_lc, ., by = "plot_id") %>%
  filter(count == max_count) %>%
  select(plot_id, lc_class_main = lc_class, nb_subplot_lc_main = max_count)

table(plot_lc$nb_subplot_lc_main)

plot_check <- plot %>%
  select(plot_id, lc_class) %>%
  left_join(plot_lc, by = "plot_id") %>%
  mutate(lc_check = if_else(lc_class == lc_class_main, 1, 0))

table(plot_check$lc_class)
table(plot_check$lc_class_main)
table(plot_check$lc_check)

test <- plot_check %>% filter(lc_check == 0)



## Tree checks ##############################################################

## Big tree images ----------------------------------------------------------

tree_big <- tree %>%
  filter(tree_dbh >= 100) %>%
  mutate(tree_dbh100check = if_else(is.na(tree_dbh100img), 0, 1))

table(tree_big$tree_dbh100check)



## Tree diameter range ------------------------------------------------------

gr_dbhrange <- tree %>%
  left_join(subplot, by = "subplot_id") %>%
  filter(lc_class %in% c("CF", "EF", "DD", "MCB", "MDF")) %>%
  ggplot(aes(x = lc_class, y = tree_dbh, fill = lc_class)) +
  geom_boxplot() +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
print(gr_dbhrange)

gr_dbhrange2 <- tree %>%
  left_join(subplot, by = c("plot_id", "subplot_id", "subplot_no")) %>%
  filter(lc_class %in% c("CF", "EF", "DD", "MCB", "MDF")) %>%
  ggplot(aes(x = plot_no, y = tree_dbh, fill = lc_class)) +
  geom_point(alpha = 0.1, shape = 21) +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
print(gr_dbhrange2)

## Deadwood checks ##########################################################

## TBD



## Stump checks #############################################################

## TBD






