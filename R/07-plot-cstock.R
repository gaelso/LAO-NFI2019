## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Propagate subplot Cstock to plot level
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

plot_simple <- plot %>%
  select(plot_id, lc_class_main = lc_class)


## Version 1 all subplots even if different 
plot_agb <- subplot_agb %>%
  left_join(plot_simple, by = "plot_id") %>% 
  group_by(lc_class_main, plot_id, plot_no) %>%
  summarise(
    n_subplot    = n(),
    plot_density = mean(n_trees),
    plot_ba      = mean(subplot_ba),
    plot_agb     = mean(subplot_agb),
    plot_bgb     = mean(subplot_bgb),
    plot_carbon  = mean(subplot_carbon)
  ) %>%
  ungroup() %>%
  filter(!is.na(lc_class_main))

## Checks
summary(plot_agb)
table(plot_agb$lc_class_main, useNA = "always")

plot_agb %>% 
  ggplot(aes(x = plot_ba, y = plot_agb, color = lc_class_main, shape = lc_class_main)) +
  geom_point(size = 2)

## Version 2: only subplots of main plot Land cover
plot_agb2 <- subplot_agb %>%
  left_join(plot_simple, by = "plot_id") %>% 
  filter(lc_class == lc_class_main) %>%
  group_by(lc_class_main, plot_id) %>%
  summarise(
    n_subplot   = n(),
    plot_density = mean(n_trees),
    plot_ba      = mean(subplot_ba),
    plot_agb     = mean(subplot_agb),
    plot_bgb     = mean(subplot_bgb),
    plot_carbon  = mean(subplot_carbon)
  ) %>%
  ungroup() %>%
  filter(!is.na(lc_class_main))

## Checks
summary(plot_agb2)
table(plot_agb2$n_subplot, useNA = "always")
table(plot_agb2$lc_class_main, useNA = "always")

plot_agb2 %>% 
  ggplot(aes(x = plot_ba, y = plot_agb, color = lc_class_main, shape = lc_class_main)) +
  geom_point()

plot_agb2 %>% 
  ggplot(aes(x = plot_ba, y = plot_agb, color = lc_class_main, shape = lc_class_main)) +
  geom_point()

## Compare
sum(plot_agb$n_subplot)
sum(plot_agb2$n_subplot)


