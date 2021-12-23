## LAO-NFI2019
## Rscripts for calculating national level carbon stock from the NFI 2019 in Lao PDR
## Initiated in March 2021

## maps
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## Maps needs spatial data and functions for mapping
## {sf} provides commands to convert data frames or tibbles to **Simple Features**
## **Simple Features** is a spatial data format that contains:
## - the attribute table as a tibble, compatible with most {tidyverse} functions
## - the polygons, lines or points coordinates stored as geometries
## - the Coordinate reference system number (ex. WGS84 CRS is 4326).
## The function st_as_sf() can convert non-spatial data to spatial data
## The function st_read() can load various spatial data format.
## {ggplot2} contains geom_sf() to map sf objects.

## Check plot tables
plot_agb %>% arrange(plot_id)
plot

## Create a tables with plot coordinates
plot_coords <- plot %>% select(plot_id, longitude, latitude, province)


## By convention I prefer to start spatial object with the prefix "sf_"
sf_plot <- plot_agb %>%
  left_join(plot_coords, by = "plot_id") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
sf_plot

## Read a shapefile from the data subfolder
sf_country <- st_read("data/LAO_adm/LAO_adm0.shp")
sf_country

## First map
ggplot(sf_plot, aes(color = plot_agb)) + 
  geom_sf()


## ggplot2 customization work too
ggplot(sf_plot, aes(fill = plot_agb)) + 
  geom_sf(shape = 21, size = 2) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "AGB (t/ha)")

## Add country boundaries
ggplot() + 
  geom_sf(data = sf_plot, aes(fill = plot_agb), shape = 21, size = 2) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "AGB (t/ha)")

## The bounding box contains the min and max of the sf object
st_bbox(sf_plot)

## ggplot maps can have different bounding box
ggplot() +
  geom_sf(data = sf_plot, aes(fill = plot_agb), shape = 21, size = 2) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_fill_viridis_c() +
  labs(fill = "AGB (t/ha)") +
  coord_sf(xlim = c(100, 104), ylim = c(19, 23))

## Map land cover class
ggplot() +
  geom_sf(data = sf_plot, aes(fill = lc_class_main, size = plot_agb), shape = 21) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(fill = "Land cover", size = "AGB (t/ha)")

## {dplyr} functions can be applied easily
sf_plot %>%
  filter(lc_class_main %in% c("CF", "EF")) %>%
  ggplot() +
  geom_sf(aes(fill = lc_class_main, size = plot_agb), shape = 21) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(fill = "Land cover", size = "AGB (t/ha)")

## Add north arrow and scale with {ggspatial}
ggplot() +
  geom_sf(data = sf_plot, aes(fill = lc_class_main, size = plot_agb), shape = 21) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_fill_viridis_d() +
  labs(
    fill = "Land cover", 
    size = "AGB (t/ha)", 
    title = "Plot mean aboveground biomass", 
    subtitle = "from NFI cycle III"
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


## map plot id instead of circles
ggplot() +
  geom_sf_text(data = sf_plot, aes(color = plot_agb, label = plot_no), size = 3) +
  geom_sf(data = sf_country, fill = NA) +
  theme_bw() +
  scale_color_viridis_c() +
  labs(color = "AGB (t/ha)")