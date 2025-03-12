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

#--------------------------------------------------------------------------

install.packages("tmap")
library(tmap)


#qtm("Ontario", bbox = c(-82, 42, -79, 46))

data("World")
tmap_mode("view")
tm_shape(World, bbox = c(-85, 40, -75, 48)) +
  tm_polygons()+
  tm_layout("Simplification: 2")

#--------------------------------------------------------------------------
library(maps)
library(ggplot2)
library(dplyr)

world_map <- map_data("world")
canada <- world_map %>% filter(region == "Canada")

p <- ggplot() + coord_fixed() +
  xlab("") + ylab("")

base_canada_messy <- p + geom_polygon(data=canada, aes(x=long, y=lat, group=group), 
                                     colour="light green", fill="light green")

base_canada_messy

#-----------------------------------------------------------------------

library(ggplot2)
library(nominatimlite)

# Extract the bounding box of a sf object
sfobj <- geo_lite_sf("canada", points_only = FALSE)
sfobj

#create bounding box
#bbox <- sf::st_bbox(sfobj)
bbox <- c(-85, 40, -75, 48)
bbox

# turn into a polygon
bbox_sfobj <- bbox_to_poly(bbox)

ggplot(bbox_sfobj) +
  geom_sf(fill = "lightblue", alpha = 0.5) +
  geom_sf(data = sfobj, fill = "wheat")

#-------------------------------------------------------------------------
library(sf)
library(tmap)
library(dplyr)
#library(rmapshaper)

#get shape file from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21

#import shape file
sfcoll <- read_sf("C:\\Users\\ameus\\Documents\\Mandeville_lab_grad\\Binf_work\\lfed000b21g_e\\lfed000b21g_e.gml")

#import list of site coordinates
coords <- read.csv("AMP22_site_coordinates.csv")
coords <- coords[-15,]
coords$Year <- as.character(coords$Year)

#change coordinates into simple features and change coordinate type
coords_sf <- st_as_sf(coords, coords = c("Long", "Lat"), crs = "EPSG:4326") %>% st_transform('EPSG:3347')

#create bbox for my area
southern_ontario_bbox = st_bbox(c(xmin = -84,
                             xmax = -76,
                             ymin = 41.737,
                             ymax = 45.1),
                           crs='EPSG:4326') %>%
  st_as_sfc() %>% st_transform('EPSG:3347')

#select only the regions that intersect my bbox
SO_intersects <- sfcoll[southern_ontario_bbox,,op=st_intersects]

#simplify borders so it doesn't take hours to plot
SO_intersects_simple <- st_simplify(SO_intersects, preserveTopology = F, 690)

# all of these options either put the file in the wrong format (sfc instead of sf) or delete all the data from the file
#SO_intersects_union <- SO_intersects_simple %>% group_by(PRUID) %>% summarise()
#SO_intersects_union <- st_union(st_combine(SO_intersects_simple), by_feature = T)
#SO_intersects_union <- SO_intersects_union %>% st_sf %>% st_cast


#plot
(map <- tm_shape(SO_intersects_simple$geometry) +
  tm_borders() +
  tm_shape(coords_sf) +
  tm_dots(size = 0.2, col = "Year", shape = 3, palette = c("#df5454", "#33a2ff")) +
  tm_scale_bar() +
  tm_compass(type = "4star", size = 1, position = c("right", "top"))+
  tm_layout(main.title = "Map of Sampling Sites", legend.title.size = 1.5, legend.text.size = 1))

#pdf("Map_of_sampling_sites.pdf")
#map
#dev.off()

#-------------------------------------------------------------------------
# THIS IS WHERE THE CODE STARTS THAT I ACTUALLY USED
library(sf)
library(tmap)
library(dplyr)

#get shape file from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21

#import shape file
sfcoll2 <- read_sf("C:\\Users\\ameus\\Documents\\Mandeville_lab_grad\\Binf_work\\lcsd000b21a_e\\lcsd000b21a_e.shp")

#import list of site coordinates
coords <- read.csv("AMP22_site_coordinates.csv")
sites <- read.table("List_of_25_sampling_sites.txt", header = T)
coords <- coords[-15,]
coords <- merge(coords, sites, by.x = "Code", by.y = "Waterbody_Code")
coords$Year <- as.character(coords$Year)

#change coordinates into simple features and change coordinate type
coords_sf <- st_as_sf(coords, coords = c("Long", "Lat"), crs = "EPSG:4326") %>% st_transform('EPSG:3347')

#create bbox for my area
southern_ontario_bbox = st_bbox(c(xmin = -83,
                                  xmax = -76,
                                  ymin = 42,
                                  ymax = 45.5),
                                crs='EPSG:4326') %>% st_as_sfc() %>% st_transform('EPSG:3347')

#select only the regions that intersect my bbox
SO_intersects2 <- sfcoll2[southern_ontario_bbox,,op=st_intersects]

#simplify borders so it doesn't take hours to plot
SO_intersects_simple2 <- st_simplify(SO_intersects2, preserveTopology = F, 420)

#remove annoying islands
SO_intersects_simple2 <- SO_intersects_simple2 %>% filter(CSDNAME != "Northeastern Manitoulin and the Islands")

#plot
set.seed(31) #something about auto.placement regenerates differently each time if not setting seed
(map2 <- tm_shape(SO_intersects_simple2$geometry) +
    tm_borders() +
    tm_shape(coords_sf) +
    #tm_text(text = "Code", col = "Year", size = 0.6, palette = c("#C70039", "#33a2ff"), bg.color = "black", bg.alpha = 0.5)+
    tm_dots(size = 0.25, col = "Year", shape = 21, palette = c("#C70039", "#33a2ff"), border.col = "black", jitter = 0) +
    tm_text("Code", col = "black", size = 0.9, just = "top", ymod = 0.8, auto.placement = T, fontfamily = "sans", fontface = "bold") +
    tm_scale_bar(position = c("right", "bottom")) +
    tm_compass(type = "4star", size = 1, position = c("left", "top"))+
    tm_layout(legend.title.size = 1.9, legend.text.size = 1.3))

# pdf("Map_of_sampling_sites_new.pdf", height = 12, width = 12)
# map2
# dev.off()

#plot w/out names on map
set.seed(40) #something about auto.placement regenerates differently each time if not setting seed
(map_noname <- tm_shape(SO_intersects_simple2$geometry) +
    tm_borders() +
    tm_shape(coords_sf) +
    #tm_text(text = "Code", col = "Year", size = 0.6, palette = c("#C70039", "#33a2ff"), bg.color = "black", bg.alpha = 0.5)+
    tm_dots(size = 0.5, col = "Year", shape = 21, palette = c("#C70039", "#33a2ff"), border.col = "black", jitter = 0.25) +
    #tm_text("Code", col = "black", size = 0.9, just = "top", ymod = 0.8, auto.placement = T, fontfamily = "sans", fontface = "bold") +
    tm_scale_bar(position = c("right", "bottom")) +
    tm_compass(type = "4star", size = 1.5, position = c("left", "top"))+
    tm_layout(legend.title.size = 2, legend.text.size = 1.5))


pdf("Map_of_sampling_sites_noname.pdf", height = 10, width = 10)
map_noname
dev.off()



# create a big canada map for behind my smaller map
# dowload province level from census canada
#import shape file
sfcoll_canada <- read_sf("C:\\Users\\ameus\\Documents\\Mandeville_lab_grad\\Binf_work\\lpr_000b21a_e\\lpr_000b21a_e.shp")

canada_simple <- st_simplify(sfcoll_canada, preserveTopology = F, 690)

(canada <- tm_shape(canada_simple$geometry)+
    tm_borders())


jpeg("Map_of_canada.jpeg")
canada
dev.off()

