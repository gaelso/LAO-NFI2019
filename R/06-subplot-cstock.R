## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Propagate tree AGB to subplot Cstock
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Summarise tree to subplot
subplot_agb <- tree_agb %>%
  group_by(subplot_id) %>%
  summarise(
    n_trees =  sum(scale_factor),
    subplot_ba = sum(tree_ba * scale_factor),
    subplot_agb = sum(tree_agb * scale_factor * 0.001)
  ) %>%
  left_join(subplot, by = "subplot_id") %>%
  mutate(
    subplot_bgb = case_when(
      lc_class == "CF" & subplot_agb <   50 ~ subplot_agb * 0.46,
      lc_class == "CF" & subplot_agb <= 150 ~ subplot_agb * 0.32,      
      lc_class == "CF" & subplot_agb >  150 ~ subplot_agb * 0.23,
      lc_class != "CF" & subplot_agb <  125 ~ subplot_agb * 0.20,
      lc_class != "CF" & subplot_agb >= 125 ~ subplot_agb * 0.24,
      TRUE ~ 0 ## <- no BGB for other cases, i.e. dead trees in the complete analysis
    ) ,
    subplot_carbon = (subplot_agb + subplot_bgb) * 0.47
  ) %>%
  filter(subplot_measured == "yes")

## Checks
subplot_agb %>%
  ggplot(aes(x = subplot_agb, y = lc_class, color = lc_class)) +
  geom_boxplot()

subplot_agb %>%
  ggplot(aes(x = subplot_ba, y = subplot_agb, fill = lc_class)) +
  geom_point(alpha = 0.8, shape = 21, size = 2) +
  scale_fill_viridis_d() +
  labs(x = "Basal Area (m2/ha)", y = "Aboveground biomass (t/ha)", color = "Land cover")

