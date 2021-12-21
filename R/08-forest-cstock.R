## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## Propagate plot Cstock to forest type
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ftype_agb <- plot_agb %>%
  group_by(lc_class_main) %>%
  summarise(
    n_plot    = n(),
    n_subplot = sum(n_subplot),
    agb       = round(mean(plot_agb), 3),
    bgb       = round(mean(plot_bgb), 3),
    carbon    = round(mean(plot_carbon), 3),
    sd_carbon = sd(plot_carbon)
  ) %>%
  mutate(
    ci       = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_perc  = round(ci / carbon * 100, 0)
  )
ftype_agb

## Requires ggtext
# my_cap <- 'xx: Total number of plots<br> <span style="color:grey;">xx: Total number of subplots</span>'

gr_ftype_agb <- ftype_agb %>%
  ggplot(aes(x = lc_class_main, y = carbon)) +
  geom_col(aes(fill = lc_class_main), col = "grey25") +
  geom_errorbar(aes(ymin = carbon + ci, ymax = carbon - ci, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 10, carbon + ci + 10), label = n_subplot), col = "grey50") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 20, carbon + ci + 20), label = n_plot)) +
  labs(caption = "black: nb of plots, gray: nb of subplots", subtitle = "All subplots, plot level CI") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()
print(gr_ftype_agb)


## Showing difference with subplot filtering
ftype_agb2 <- plot_agb2 %>%
  group_by(lc_class_main) %>%
  summarise(
    n_plot    = n(),
    n_subplot = sum(n_subplot),
    agb       = round(mean(plot_agb), 3),
    bgb       = round(mean(plot_bgb), 3),
    carbon    = round(mean(plot_carbon), 3),
    sd_carbon = sd(plot_carbon)
  ) %>%
  mutate(
    ci       = sd_carbon / sqrt(n_plot) * round(qt(0.975, n_plot-1), 2),
    ci_wrong = sd_carbon / sqrt(n_subplot) * round(qt(0.975, n_subplot-1), 2),
    ci_perc  = round(ci / carbon * 100, 0)
  )
ftype_agb2


gr_ftype_agb2 <- ftype_agb2 %>%
  ggplot(aes(x = lc_class_main, y = carbon)) +
  geom_col(aes(fill = lc_class_main), col = "grey25") +
  geom_errorbar(aes(ymin = carbon + ci, ymax = carbon - ci, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 10, carbon + ci + 10), label = n_subplot), col = "grey50") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 20, carbon + ci + 20), label = n_plot)) +
  labs(subtitle = "Filtered subplots, plot level CI") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

gr_ftype_agb2_wrong <- ftype_agb2 %>%
  ggplot(aes(x = lc_class_main, y = carbon)) +
  geom_col(aes(fill = lc_class_main), col = "grey25") +
  geom_errorbar(aes(ymin = carbon + ci_wrong, ymax = carbon - ci_wrong, width = 0.5), col = "grey25") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 10, carbon + ci_wrong + 10), label = n_subplot), col = "grey50") +
  geom_label(aes(y = if_else(is.na(sd_carbon), carbon + 20, carbon + ci_wrong + 20), label = n_plot)) +
  labs(subtitle = "Filtered subplots, subplot level CI") +
  theme(legend.position = "none") +
  scale_fill_viridis_d()

gr_ftype_agb_compa <- ggpubr::ggarrange(gr_ftype_agb, gr_ftype_agb2, gr_ftype_agb2_wrong, 
                                        nrow = 1, align = "hv")
print(gr_ftype_agb_compa)



