library(sf)
library(mapview)
library(lwgeom)

# OSM data
osm <- read_sf("9_archi-wiki/data/Strassburg-shp/shape/buildings.shp")

osm <- st_make_valid(osm)

plot(osm) # does not work
