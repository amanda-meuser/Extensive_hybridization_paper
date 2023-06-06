install.packages(c("rnaturalearth", "rnaturalearthdata"))
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")

canada <- ne_countries(country = "canada", scale = "large", returnclass = "sf")
class(canada)
(sites <- data.frame(longitude = c(-82, -79), latitude = c(42, 46)))

ggplot(data = canada) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-84.5, -76.5), ylim = c(41.5, 46.5), expand = FALSE)

#---------------------------------------------------------------

library(mapview)
mapview(canada)


#---------------------------------------------------------------

install.packages("canadamaps")
library(canadamaps)


#---------------------------------------------------------------
# open street maps
#install.packages("osmdata")

library(osmdata)
library(dplyr)
library(terra)
library(sf)

# set bounding box of coordinates
coords_ON <- c(-82, 42, -79, 46)
# query open street maps
ontario_map = opq(bbox = coords_ON) %>%
  add_osm_feature(key = 'boundary', value = 'administrative') %>%
  add_osm_feature(key = 'admin_level', value = '6') %>%
  osmdata_sf() %>%
  .$osm_multipolygons
#then, we can cut down the geometries using another bounding box, which we have custom made
ontario_geom = ontario_map$geom
plot(ontario_geom)
