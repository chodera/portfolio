# -----------------------------------------------------------------------
# Project: History of buildings in Strasbourg
# Title: Preparing cadastre data
# Begin: 13/05/2019
# End: xxx
# -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(sf)
library(leaflet)


# Load and prepare data ---------------------------------------------------

# Cadastre
stras_cadastre <- read_sf("9_archi-wiki/data/cadastre-parcelle/rg_cad_parcelle.shp")

# Réferentiel 1:10000
## Bâtiment individuel
stras_carto_bati <- read_sf("9_archi-wiki/data/referentiel_carto_10_000_shp/rg_c10_bati_indiv.shp")
## Adresse postale
stras_carto_adresse <- read_sf("9_archi-wiki/data/referentiel_topo_simplifie_shp/rg_c10_bati_indiv.shp")

# Réferentiel topographique
stras_topo <- read_sf("9_archi-wiki/data/referentiel_carto_10_000_shp/rg_r2m_bati_fusionne.shp")

# Filter for small part of the city (for testing)
# stras_cadastre %>% 
#   distinct(NUM_SECTIO) %>% 
#   head()

# section_33 <- stras_cadastre %>% 
#   filter(NUM_SECTIO == "33")
# 
# # Transform and set crs
# section_33 <- section_33 %>%
#   st_transform(4326) %>% 
#   st_set_crs(NA) %>%
#   st_set_crs('+proj=longlat +datum=WGS84')

st_crs(stras_carto_bati)

stras_carto_bati <- stras_carto_bati %>%
  st_transform(4326) %>%
  st_set_crs(NA) %>%
  st_set_crs('+proj=longlat +datum=WGS84')

# Plot map ----------------------------------------------------------------

# Define labels
labels <- sprintf(
  "<strong>Parcelle: %s</strong>",
  stras_carto_bati$COM_CUS
) %>% lapply(htmltools::HTML)

# Set vectore to exclude streets
streets <- c("113", "69", "127", "44", "139", "26", "105")

# Set map
map <- leaflet(stras_carto_bati) %>%
  # Set view to Strasbourg city
  setView(lng = 7.7534101, lat = 48.5812226, zoom = 14) %>% 
  addTiles() %>%
  addPolygons(
    fillColor = "green",
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    )
map

class(stras_carto_bati)
names(st_geometry(stras_carto_bati$geometry))
class(stras_carto_bati) = c("sf", "data.frame")
names(stras_carto_bati$geometry)

plot(stras_carto_bati$geometry)

library(mapview)
mapview(stras_carto_bati)

# leaflet(section) %>%
#   setView(10, 51, 3) %>%
#   addTiles() %>% 
#   addPolygons(
#     fillColor = ~pal(r_gini_post),
#     weight = 2,
#     opacity = 1,
#     color = "white",
#     dashArray = "3",
#     fillOpacity = 0.7,
#     highlight = highlightOptions(
#       weight = 5,
#       color = "#666",
#       dashArray = "",
#       fillOpacity = 0.7,
#       bringToFront = TRUE),
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")) %>% 
#   addLegend(pal = pal, values = ~r_gini_post, opacity = 0.7, title = NULL,
#             position = "bottomright")