
###
### Demo carbon stock #######################################################
###

## Steps:
## - Calculate tree AGB
## - Aggregate to subplot
## - Calculate tree BGB
## - Aggregate to plot
## - Aggregate to forest type


## --- Setup ----------------------------------------------------------------

library(tidyverse)

subplot_init <- read_csv("data/subplot_all.csv")

tree_init <- read_csv("data/tree_all.csv")
  
table(subplot_init$lc_class)

table(tree_init$class_code)
table(tree_init$class)

summary(tree_init$dbh)

summary(tree_init)


## --- Modif subplot label --------------------------------------------------

subplot <- subplot_init %>%
  mutate(
    subplot_id = str_remove(subplot_id, "sub_plot")
  )


## --- Tree AGB -------------------------------------------------------------

tree <- tree_init %>%
  left_join(subplot) %>%
  mutate(
    ba_tree = round((dbh/200)^2 * pi, 2), 
    agb_live = case_when(
      lc_class == "EF"  ~ 0.3112 * dbh^2.2331,
      lc_class == "DD"  ~ 0.2137 * dbh^2.2575,
      lc_class == "MDF" ~ 0.523081 * dbh^2,
      lc_class == "CF"  ~ 0.1277 * dbh^2.3944,
      lc_class == "MCB" ~ 0.1277 * dbh^2.3944,
      TRUE ~ NA_real_
    ),
    agb_dead1 = 0.6 * exp(-1.499 + (2.148*log(dbh)) + (0.207*(log(dbh))^2) - (0.0281*(log(dbh))^3)),
    agb_dead2short = pi * h * 100 / 12 * (dead_DB_short^2 + dead_DB_short*dead_DT_short + dead_DT_short^2) * 0.6 * 0.001, ## Conv H to cm then wd in g.cm-3 then multiply by 0.001 to kg. 
    dead_DT_tall = dead_DB_tall - (h * (dead_DB_tall - dbh) / 1.3), ## In Excel / 130 * 100
    dead_DT_tall_cor = case_when(
      class_code == 22 & h <= h_max ~ dead_DB_tall - (h     * (dead_DB_tall - dbh) / 1.3),
      class_code == 22 & h >  h_max ~ 0,
      TRUE ~ NA_real_
    ),
    agb_dead2tall = pi * h * 100 / 12 * (dead_DB_tall^2 + dead_DB_tall*dead_DT_tall_cor + dead_DT_tall_cor^2) * 0.6 * 0.001,
    agb_stump = dbh^2 * pi / 4 * h * 100 * 0.57 * 0.001, ## H harmonized to m earlier  
    agb = case_when(
      class_code ==  0 ~ agb_live,
      class_code == 10 ~ agb_live,
      class_code == 21 ~ agb_dead2short,
      class_code == 22 ~ agb_dead2tall,
      class_code == 30 ~ agb_stump,
      TRUE ~ NA_real_
    )
  )

table(tree$lc_class)
table(tree$class_code)
## Visual checks
tree_sub <- tree %>% filter(class_code == 0, lc_class == "EF")

tree %>% 
  ggplot() +
  geom_point(aes(x = dbh, y = agb, color = class, shape = class)) +
  geom_line(data = tree_sub, aes(x = dbh, y = agb))

tree %>%
  filter(class_code == 22) %>%
  ggplot(aes(x = dbh, y = agb, color = class)) +
  geom_point()

## Outlier here

tree %>%
  filter(class_code == 22) %>%
  ggplot(aes(x = dbh, y = h, color = class)) +
  geom_point()

my_col <- viridis::viridis(n = 3)

tree %>%
  filter(class_code == 22, dead_DT_tall > -100, dead_DT_tall < 100) %>%
  ggplot(aes(y = h)) +
  geom_point(aes(x = dbh), col = my_col[3]) +
  geom_point(aes(x = dead_DB_tall), col = my_col[2], pch = 17) +
  geom_point(aes(x = dead_DT_tall), col = my_col[1], pch = 3) +
  geom_vline(xintercept = 0, col = "red") +
  theme_bw() +
  labs(
    subtitle = "Dead tree class 2 tall", x = "Diameter", y = "Tree Height", 
    caption = "DBH: Yellow circle\n DB: Green triangle\n DT: purple cross"
    )


tree %>%
  filter(class_code == 22, h_max <= 50, h_max > 0) %>%
  ggplot(aes(x = dbh)) +
  geom_point(aes(y = h_max), col = "red") +
  geom_point(aes(y = h), col = "blue") +
  geom_segment(aes(xend = dbh, y = h_max, yend = h))

tree %>%
  filter(class_code == 22) %>%
  ggplot(aes(y = h, color = class)) +
  geom_point(aes(x = dbh), col = my_col[3]) +
  geom_point(aes(x = dead_DB_tall), col = my_col[2], pch = 17) +
  geom_point(aes(x = dead_DT_tall_cor), col = my_col[1], pch = 3) +
  geom_vline(xintercept = 0, col = "red") +
  theme_bw() +
  labs(
    subtitle = "Dead tree class 2 tall", x = "Diameter", y = "Tree Height", 
    caption = "DBH: Yellow circle\n DB: Green triangle\n DT: purple cross"
  )


## Check missing agb
tree_sub <- tree %>%
  filter(is.na(agb))

table(tree_sub$class, tree_sub$lc_class, useNA = "always")



## --- tree to subplot AGB --------------------------------------------------

subplot_agb <- tree %>%
  filter(!is.na(agb)) %>%
  mutate(
    ba_m2ha_live = if_else(class_code == 0, ba_tree * scale_factor, 0),
    agb_tha_all  = agb * scale_factor * 0.001,
    agb_tha_live = if_else(class_code == 0, agb_tha_all, 0),
    is_live_ha   = if_else(class_code == 0, scale_factor, 0),
    is_stump_ha  = if_else(class_code == 30,scale_factor, 0),
    ) %>%
  group_by(subplot_no) %>%
  summarise(
    n_trees = n(),
    n_live = sum(is_live_ha),
    n_stump = sum(is_stump_ha),
    ba_live = sum(ba_m2ha_live),
    agb_all = sum(agb_tha_all),
    agb_live = sum(agb_tha_live)
  ) %>%
  left_join(subplot) %>%
  mutate(
    bgb_live = case_when(
      lc_class == "CF" & agb_live <   50 ~ agb_live * 0.46,
      lc_class == "CF" & agb_live <= 150 ~ agb_live * 0.32,      
      lc_class == "CF" & agb_live >  150 ~ agb_live * 0.23,
      lc_class != "CF" & agb_live <  125 ~ agb_live * 0.20,
      lc_class != "CF" & agb_live >= 125 ~ agb_live * 0.24,
      TRUE ~ NA_real_
     ) ,
    carbon = (agb_all + bgb_live) * 0.47
  )

## Checks 
subplot_agb %>%
  ggplot(aes(x = lc_class, y = agb_all, color = lc_type)) +
  geom_boxplot()

subplot_agb %>%
  ggplot(aes(x = ba_live, y = agb_live, color = lc_class)) +
  geom_point()

subplot_agb %>%
  filter(!(lc_class %in% c("OTH", "SW"))) %>%
  ggplot(aes(x = ba_live, y = agb_all, color = lc_class)) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  labs(x = "Basal Area (m2/ha)", y = "Aboveground biomass (t/ha)", color = "Land cover")

subplot_agb %>%
  ggplot(aes(x = agb_live, y = agb_all, color = lc_class)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)


## --- Get default lc_class for each plot -----------------------------------

subplot_lc <- subplot %>%
  arrange(plot_id) %>%
  mutate(count = 1) %>%
  select(plot_id, subplot_id, lc_class, count) %>%
  distinct() %>%
  pivot_wider(names_from = subplot_id, values_from = count, values_fill = 0) %>%
  mutate(count = rowSums(x = .[3:12]))

plot_lc <- subplot_lc %>% 
  group_by(plot_id) %>%
  summarise(max_count = max(count)) %>%
  left_join(subplot_lc, .) %>%
  filter(count == max_count) %>%
  select(plot_id, lc_class_main = lc_class, nb_subplot_lc_main = max_count)

table(plot_lc$nb_subplot_lc_main)

## Add gps coordinates
plot_coord <- subplot %>% 
  filter(!is.na(GPS_latitude)) %>%
  group_by(plot_id) %>%
  summarise(
    x = mean(GPS_longitude),
    y = mean(GPS_latitude)
  ) %>%
  left_join(plot_lc)

write_csv(plot_coord, "data/plot_coord.csv")



## -- Aggregate subplots to plot --------------------------------------------

plot_agb <- subplot_agb %>%
  left_join(plot_lc) %>% 
  filter(lc_class == lc_class_main) %>%
  group_by(plot_id, lc_class_main, nb_subplot_lc_main) %>%
  summarise(
    n_trees  = mean(n_trees),
    n_live   = mean(n_live),
    n_stump  = mean(n_stump),
    ba_live  = mean(ba_live),
    agb_all  = mean(agb_all),
    agb_live = mean(agb_live),
    bgb_live = mean(bgb_live),
    carbon   = mean(carbon)
  )

## Check nb plot is consistent
plot_list <- subplot_agb %>% pull(plot_id) %>% unique() %>% sort()
plot_list2 <- plot_agb %>% pull(plot_id) %>% unique() %>% sort()

## Checks
plot_agb %>%
  ggplot(aes(x = lc_class_main, y = agb_all)) +
  geom_boxplot()

plot_agb %>% 
  filter(!(lc_class_main %in% c("OTH", "SW"))) %>%
  ggplot(aes(x = ba_live, y = agb_live, color = lc_class_main, shape = lc_class_main)) +
  geom_point()

plot_agb %>%
  filter(!(lc_class_main %in% c("OTH", "SW"))) %>%
  ggplot(aes(x = ba_live, y = agb_all, color = lc_class_main, shape = lc_class_main)) +
  geom_point()

plot_agb %>%
  ggplot(aes(x = agb_live, y = agb_all, color = lc_class_main)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)

plot_agb %>%
  ggplot(aes(x = agb_live, y = bgb_live, color = lc_class_main)) +
  geom_point()


## --- Aggregate plots to forest type ---------------------------------------

ftype <- plot_agb %>%
  group_by(lc_class_main) %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(agb_all), 3),
    agb_live   = round(mean(agb_live), 3),
    bgb_live   = round(mean(bgb_live), 3),
    carbon_tot = round(mean(carbon), 3),
    sd_carbon  = sd(carbon)
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
ftype

ftype %>%
  ggplot(aes(x = lc_class_main, y = carbon_tot)) +
  geom_col(aes(fill = lc_class_main), col = "grey25") +
  geom_errorbar(aes(ymin = carbon_tot + ci, ymax = carbon_tot - ci, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon_tot + 10, carbon_tot + ci + 10), label = n_plot))


## --- Aggregate plots to strata --------------------------------------------

table(plot_agb$lc_class_main, useNA = "always")

strata <- plot_agb %>%
  filter(!(lc_class_main %in% c("OTH", "SW"))) %>%
  mutate(
    strata = case_when(
      lc_class_main == "EF"                    ~ "Stratum 1",
      lc_class_main %in% c("MDF", "CF", "MCB") ~ "Stratum 2",
      lc_class_main == "DD"                    ~ "Stratum 3",
      TRUE ~ NA_character_
      )
    ) %>%
  group_by(strata) %>%
  summarise(
    n_plot     = n(),
    agb_all    = round(mean(agb_all), 3),
    agb_live   = round(mean(agb_live), 3),
    bgb_live   = round(mean(bgb_live), 3),
    carbon_tot = round(mean(carbon), 3),
    sd_carbon  = sd(carbon)
  ) %>%
  mutate(
    ci      = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc = round(ci / carbon_tot * 100, 0)
  )
strata

strata %>%
  ggplot(aes(x = strata, y = carbon_tot)) +
  geom_col(aes(fill = strata), col = "grey25") +
  geom_errorbar(aes(ymin = carbon_tot + ci, ymax = carbon_tot - ci, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon_tot + 10, carbon_tot + ci + 10), label = n_plot)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "", y = "Carbon stock (t/ha)", caption = "Number of plots indicated in the boxes")
  



