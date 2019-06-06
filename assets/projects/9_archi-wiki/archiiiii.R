# Description -------------------------------------------------------------
# Project: History of buildings in Strasbourg
# Title: Preparing cadastre data
# Begin: 13/05/2019
# End: xxx


# Result: it is gonna work!
# Still, there is loads of work to do.
# 1. Get all archi-wiki data (with lan and lon to see whether the results differ when using the referentiel)
# -> DONE
# 2. Test lan and lot from meta data
# -> DONE, result not satisfying -> go with referentiel
# 3. Harmonize archi wiki addresses and referentiel 
# -> PARTIALLY DONE (lots of manual work ahead)
# 4. Visualize data
# -> DONE
# to do: check links (special characters), check bins (interval borders), harmonize addresses


# Structure ---------------------------------------------------------------

# 1. Prepare Archi-Wiki data
# 2. Geocode addresses
# 3. Prepare building footprints for Strasbourg
# 4. Merge data
# 5. Plot data


# Load packages -----------------------------------------------------------

library(tidyverse)
library(sf)
library(leaflet)
library(mapview)
library(lwgeom)
library(RColorBrewer)
# library(httr)
# library(jsonlite)


# 1. Prepare Archi-Wiki data ----------------------------------------------

# Load seperate data frames
archiwiki_1 <- read_rds("Berlin/archi-wiki_1.rds") %>% 
  rename(adresse = `link_texts[1:2600]`,
         date = date_full,
         structure = structure_full, 
         urls = `urls[1:2600]`,
         lat = lat_full,
         lon = lon_full)
archiwiki_2 <- read_rds("Berlin/archi-wiki_2.rds") %>% 
  slice(2601:5200) %>% 
  rename(adresse = `link_texts[1:5200]`,
         date = date_full,
         structure = structure_full, 
         urls = `urls[1:5200]`,
         lat = lat_full,
         lon = lon_full)
archiwiki_3 <- read_rds("Berlin/archi-wiki_3.rds") %>% 
  rename(adresse = `link_texts[5201:7800]`,
         date = date_full,
         structure = structure_full, 
         urls = `urls[5201:7800]`,
         lat = lat_full,
         lon = lon_full)
archiwiki_4 <- read_rds("Berlin/archi-wiki_4.rds") %>% 
  rename(adresse = `link_texts[7801:length(urls)]`,
         date = date_full,
         structure = structure_full, 
         urls = `urls[7801:length(urls)]`,
         lat = lat_full,
         lon = lon_full)

# Join separate date frames
archiwiki <- bind_rows(archiwiki_1, 
                       archiwiki_2, 
                       archiwiki_3, 
                       archiwiki_4)

# Clean data set
archiwiki_clean <- archiwiki %>% 
  # Adresse
  mutate(adresse = str_replace_all(adresse, c("Adresse:" = "", 
                                              " \\(Strasbourg\\)" = "")),
         ## Remove special characters
         adresse = iconv(adresse, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         ## Convert to upper case
         adresse = str_to_upper(adresse)) %>% 
  # Date
  mutate(date = str_replace_all(date, c("Date de construction " = "", 
                                        "environ " = "",
                                        "vers " = "",
                                        "au " = "",
                                        "novembre " = "",
                                        " \\(en cours\\)" = "",
                                        "à " = "",
                                        "[0-9]{1,2}/" = "")),    
         date = str_trim(date, side = "both")) %>% 
  separate(date, c("start", "end"), sep = " ", remove = FALSE, fill = "right") %>% 
  mutate(date = str_replace(date, " ", "-")) %>% 
  # Structure
  mutate(structure = str_replace(structure, "Structure ", ""),
         structure = str_trim(structure, side = "both"))

archiwiki_clean2 <- archiwiki_clean %>% 
  # Remove double addresses (keep only first one)
  mutate(adresse = str_replace_all(adresse, ",.*$", "")) %>% 
  # Remove multiple house numbers (keep only first one) 
  mutate(adresse = str_replace_all(adresse, "-[0-9]*", ""))

# Drop missing values for location
archiwiki_clean2 <- archiwiki_clean2 %>% 
  filter(lat != "" & lon != "")

# Remove data frames which are not used any more
rm(archiwiki, archiwiki_1, archiwiki_2, archiwiki_3, archiwiki_4)


# 2. Geocode addresses ----------------------------------------------------

# Referentiel adresse
# Source: https://data.strasbourg.eu/explore/dataset/referentiel-adresse/

# # API: auf 10000 Zeilen begrenzt, evtl for loop schreiben, um alle Daten zu bekommen
# # Create response object
# url <- "https://data.strasbourg.eu/api/records/1.0/search/?dataset=referentiel-adresse&facet=numero&facet=nom_rue&facet=nom_commun"
# raw.result  <- GET(url)
# 
# # Check response object
# status_code(raw.result)
# 
# # Access raw json data and transform into data frame
# json_text <- content(raw.result, "text")
# names(fromJSON(json_text))
# records <- fromJSON(json_text)$records
# data <- records$fields

# From csv file
referentiel <- read_csv2("Berlin/referentiel-adresse.csv")

referentiel <- referentiel %>% 
  # Filter for Strasbourg
  filter(NOM_COMMUN == "Strasbourg") %>% 
  # Unite house number and street name
  unite("adresse", c("NUMERO", "NOM_RUE"), sep = " ") %>% 
  mutate(adresse = str_to_upper(adresse)) %>% 
  # Select address and location
  select(adresse, location = `Geo Point`)

# Merge data
full <- left_join(archiwiki_clean2, referentiel, by = "adresse") %>% 
  select(adresse, location, date, urls) %>% 
  drop_na(location)


# 3. Get building footprints for Strasbourg -------------------------------

# Stras OSM data (more than Stras!)
# Source: https://download.bbbike.org/osm/bbbike/
osm <- read_sf("Berlin/Strassburg-shp/shape/buildings.shp")
osm <- st_make_valid(osm) # some polygons need to be repaired
osm <- osm %>%
  st_transform(4326) %>%
  st_set_crs(NA) %>%
  st_set_crs('+proj=longlat +datum=WGS84')

## Get Strasbourg boundaries
# Source: https://data.strasbourg.eu/explore/dataset/referentiel-topographique-simplifie/
cus_boundaries <- read_sf("Berlin/Strassburg-shp-boundaries/rg_r2m_commune.shp")
# Filter for Strasbourg
stras_boundaries <- cus_boundaries %>% 
  filter(NOM_COM == "Strasbourg")
stras_boundaries <- stras_boundaries %>%
  st_transform(4326) %>%
  st_set_crs(NA) %>%
  st_set_crs('+proj=longlat +datum=WGS84')

## Reduce Stras OSM to Strasbourg
in_stras <- st_contains(stras_boundaries, osm) %>% 
  unlist()
osm_stras <- osm %>% 
  slice(in_stras)


# 4. Merge data -----------------------------------------------------------

# Create x and y coordinates
full_final <- full %>%
  separate(location, into = c("y", "x"), sep = ", ")

# Add id column to osm data
osm_stras <- osm_stras %>% 
  mutate(id=seq(1:nrow(osm_stras))) 


# convert points into sf object
points <- st_as_sf(full_final,
                   coords = c("x", "y"))
st_crs(points) <- 4326
points <- points %>%
  st_transform(4326) %>%
  st_set_crs(NA) %>%
  st_set_crs('+proj=longlat +datum=WGS84')

#find intersection
joined <- osm_stras %>% 
  st_intersection(points) 
joined <- joined %>% 
  select(osm_id, adresse, date, urls)
joined$geometry <- NULL

# Merge back
osm_stras2 <- left_join(osm_stras, joined, by = "osm_id")
osm_stras2 <- osm_stras2 %>% 
  drop_na(date)

# Simplify
#osm_stras2 <- rmapshaper::ms_simplify(osm_stras2)


# 5. Plot map ----------------------------------------------------------------

# Final clean-up
osm_stras2$date <- as.integer(osm_stras2$date)
summary(osm_stras2$date)

testing <- osm_stras2 %>% filter(date == 191933) # weiter oben checken!

osm_stras2 <- osm_stras2 %>% 
  filter(date < 2050)

# Define colors
bins <- c(1200, 1599, 1700, 1800, 1870, 1918, 1945, 1970, 2050)
pal <- colorBin("YlGnBu", domain = osm_stras2$date, bins = bins, reverse = TRUE)
colors <- brewer.pal(8, "YlGnBu") %>% 
  rev()
labels_legend <- c("avant 1600", "1600 - 1700", "1701 - 1800", "1801 - 1870", "1871 - 1918", "1919 - 1945", "1946 - 1970", "après 1970")
  
# Define labels
labels <- sprintf(
  "<strong>%s</strong>
  <br/>Date de construction: %g
  <br/>En savoir plus: <a target='_blank' href=%s>Archi-Wiki</a>",
  osm_stras2$adresse, osm_stras2$date, osm_stras2$urls
) %>% lapply(htmltools::HTML)

# Set map
map <- leaflet(st_sf(osm_stras2)) %>%
  # Set view to Strasbourg city
  setView(lng = 7.7534101, lat = 48.5812226, zoom = 14) %>% 
  # Add tiles from Carto
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>% 
  # Add polygons
  addPolygons(
    fillColor = ~pal(date),
    weight = 2,
    opacity = 1,
    color = ~pal(date),
    dashArray = "1",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    #label = labels, # with this option infobox appears after hovering
    popup = labels, # with this option infobox appears after clicking
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
  # addLegend(pal = pal, values = ~date, opacity = 0.7, title = NULL,
  #           position = "bottomright")
  addLegend(colors = colors, labels = labels_legend, opacity = 0.7, title = NULL,
            position = "bottomright")

# Option to open map in browser
options("viewer" = function(url, ...) utils::browseURL(url))

# Plot map
map

# Other possible tiles:
# 1. Thunderforest.Pioneer (API key neeeded)
# addTiles(
#   urlTemplate = "https://{s}.tile.thunderforest.com/pioneer/{z}/{x}/{y}.png?apikey={apikey}",
#   attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
#   options = tileOptions(apikey = "424453448a014e30955172191b114137"))
# 2. addProviderTiles(providers$Stamen.TonerLite)
# 3. addProviderTiles(providers$Esri.WorldGrayCanvas)
# 4. addProviderTiles(providers$CartoDB.PositronNoLabels)
# 5. addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
