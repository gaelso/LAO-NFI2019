## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## maps - part 2 Lao language
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## **************************************************************************
## !!! For demonstration only - this chunk should be moved to the 00-libraries script 

## First, we need to create a folder font and download a font compatible with Lao
## For example the font Sengbuhan from this website: https://laoscript.net/download
## We can do it manually or automatically with the R function download.file()
## We also add a if statement to only download the font if not already in the fonts folder

dir.create("fonts", showWarnings = F)

if (!("Sengbuhan.ttf" %in% list.files("fonts"))) {
  
  download.file(
    url = paste0("https://laoscript.net/download/sengbuhan.ttf"), 
    destfile = file.path("fonts", "Sengbuhan.ttf"),
    mode = "wb"
  )
  
}

## We then need the library {showtext} to load the font into R and make it available
## For ggplot()

library(showtext)

font_add("Sengbuhan", "fonts/Sengbuhan.ttf")
showtext_auto()

## **************************************************************************



## We are now ready to add it to ggplot() via theme()
ggplot() +
  geom_sf(data = sf_plot, aes(fill = lc_class_main, size = plot_agb), shape = 21) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  theme(text = element_text(family = "Sengbuhan", size = 16)) +
  scale_fill_viridis_d() +
  labs(
    fill = "ວິທີເຮັດເສັ້ນສະແດງ", 
    size = "ວິທີເຮັດເສັ້ນສະແດງ", 
    title = "ວິທີເຮັດເສັ້ນສະແດງ", 
    subtitle = "ວິທີເຮັດເສັ້ນສະແດງ"
  ) +
  ggspatial::annotation_scale(
    location = "tr",
    bar_cols = c("grey60", "white")
  ) +
  ggspatial::annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    pad_x = unit(0.2, "in"), 
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20"
    )
  )
