# Script for creating a map of  sampling sites - Amanda Meuser
#----------------------------------------------------
library(sf)
library(tmap)
library(dplyr)
library(ggplot2)
library(ggrepel)

#get shape file from: https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# select cartographic boundary files (CBF) > administrative boundaries > census subdivisions > shapefile (.shp)

#import shape file
sfcoll2 <- read_sf("/Users/ameuser/Library/CloudStorage/GoogleDrive-ameuser03@gmail.com/My Drive/PhD_Files/Masters_stuff/Thesis_pub/lcsd000b21a_e/lcsd000b21a_e.shp")

#import list of site coordinates
coords <- read.csv("AMP22_site_coordinates.csv")
sites <- read.table("List_of_25_sampling_sites.txt", header = T)
coords <- coords[-15,] #remove double COS site
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

coords_sf <- coords_sf %>%
  mutate(
    label_x = st_coordinates(geometry)[, 1] + runif(n(), -1, 1),  # Random offset for example
    label_y = st_coordinates(geometry)[, 2] + runif(n(), -1, 1)   # Random offset for example
  )

#plot
# set.seed(31) #something about auto.placement regenerates differently each time if not setting seed
# (map2 <- tm_shape(SO_intersects_simple2$geometry) +
#     tm_borders() +
#     tm_shape(coords_sf) +
#     tm_symbols(size = 0.6, fill = "Year", col = "Year", palette = c("#C70039", "#33a2ff"), shape = 21) +
#     #tm_text(text = "Code", col = "Year", size = 0.6, palette = c("#C70039", "#33a2ff"), bg.color = "black", bg.alpha = 0.5)+
#     #tm_dots(size = 0.5, col = "Year", shape = 21, palette = c("#C70039", "#33a2ff"), border.col = "black", jitter = 25) +
#     tm_text(text = "Code", xmod.scale = "label_x", ymod.scale = "label_y", col = "black", size = 0.8, ymod = 0.8, fontfamily = "sans", fontface = "bold") +
#     tm_scalebar(position = c("right", "bottom")) +
#     tm_compass(type = "4star", size = 1.5, position = c("left", "top"))+
#     tm_layout(legend.title.size = 1.9, legend.text.size = 1.3))
# 


(map3 <- ggplot() +
  geom_sf(data = SO_intersects_simple2, fill = "white", color = "black") +
  geom_point(data = coords_sf, 
             aes(x = label_x, y = label_y, fill = Year, shape = Year), color = "black", size = 3) + 
  geom_text_repel(data = coords_sf, 
                  aes(x = label_x, y = label_y, label = Code),  
                  size = 3, box.padding = 0.3, max.overlaps = 50, fontface = "bold") +  # Increase max.overlaps to allow more labels
  scale_fill_manual(values = c("#C70039", "#33a2ff")) +  
  scale_shape_manual(values = c(24,21)) + 
  labs(color = "Year", shape = "Year") +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()))

# pdf("Map_of_sampling_sites_ggplot.pdf", height = 12, width = 12)
# map3
# dev.off()

#plot w/out names on map
# set.seed(40) #something about auto.placement regenerates differently each time if not setting seed
# (map_noname <- tm_shape(SO_intersects_simple2$geometry) +
#     tm_borders() +
#     tm_shape(coords_sf) +
#     #tm_text(text = "Code", col = "Year", size = 0.6, palette = c("#C70039", "#33a2ff"), bg.color = "black", bg.alpha = 0.5)+
#     tm_dots(size = 0.5, col = "Year", shape = 21, palette = c("#C70039", "#33a2ff"), border.col = "black", jitter = 0.25) +
#     #tm_text("Code", col = "black", size = 0.9, just = "top", ymod = 0.8, auto.placement = T, fontfamily = "sans", fontface = "bold") +
#     tm_scalebar(position = c("right", "bottom")) +
#     tm_compass(type = "4star", size = 1.5, position = c("left", "top"))+
#     tm_layout(legend.title.size = 2, legend.text.size = 1.5))


(map4 <- ggplot() +
    geom_sf(data = SO_intersects_simple2, fill = "#cadcb3", color = "black") +
    geom_point(data = coords_sf, 
               aes(x = label_x, y = label_y, fill = Year, shape = Year), color = "black", size = 3) + 
    scale_fill_manual(values = c("#C70039", "#33a2ff")) +  
    scale_shape_manual(values = c(24,21)) + 
    labs(color = "Year", shape = "Year") +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank()))


# pdf("Map_of_sampling_sites_noname.pdf", height = 10, width = 10)
# map4 
# dev.off()



# create a big canada map for behind my smaller map
# dowload province level from census canada
#import shape file
sfcoll_canada <- read_sf("C:\\Users\\ameus\\Documents\\Mandeville_lab_grad\\Binf_work\\lpr_000b21a_e\\lpr_000b21a_e.shp")

canada_simple <- st_simplify(sfcoll_canada, preserveTopology = F, 690)

(canada <- tm_shape(canada_simple$geometry)+
    tm_borders())


# jpeg("Map_of_canada.jpeg")
# canada
# dev.off()

