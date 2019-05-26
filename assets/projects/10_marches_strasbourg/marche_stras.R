# Load packages
library(tidyverse)
library(rvest)
library(nominatim)
library(leaflet)

# Specify the url for desired website to be scraped
url <- "https://www.strasbourg.eu/marches-brocantes"

# Read the HTML code from the website
page <- read_html(url)

# Get information from website
marche_stras <- html_nodes(page, "div.rte") %>% 
  html_children() %>% 
  html_text() 

# Clean data
## Split strings if they contain several markets
marche_stras[7] <- str_split(marche_stras[7], "(?=(Boulevard |Place de ))")
marche_stras[9] <- str_split(marche_stras[9], " et ")
marche_stras[12] <- str_split(marche_stras[12], "(?=(Place de Haldenbourg |Rue de |Place de Zurich |Rue Watteau ))")
marche_stras[20] <- str_split(marche_stras[20], "(?=Livres )")
marche_stras[20][[1]][2] <- str_split(marche_stras[20][[1]][2], " et ")
marche_stras[23] <- str_split(marche_stras[23], "(?=(Route d'Altenheim |Allée |Place d'Ostwald |Place du Corps |Place de l'Esplanade ))")
marche_stras[26] <- str_split(marche_stras[26], "(?=Place de )")
marche_stras[35] <- str_split(marche_stras[35], "(?=(Boulevard |Place de |Place du Corps |Rue Watteau |Producteurs |Place du Maillon))")
marche_stras[39] <- str_split(marche_stras[39], "(?=Livres )")
marche_stras[39][[1]][2] <- str_split(marche_stras[39][[1]][2], " et ")
marche_stras <- unlist(marche_stras)

## Turn into tibble and drop unecessary rows
marche_stras <- marche_stras %>% 
  tibble() %>% 
  slice(-c(1, 65:72)) 

## Create column day
marche_stras <- marche_stras %>% 
  mutate(day = marche_stras$.)
marche_stras$day[!marche_stras$day %in% c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi")] <- NA
marche_stras <- marche_stras %>% 
  fill(day)

## Create column hour
marche_stras <- marche_stras %>% 
  mutate(hour = marche_stras$.)
marche_stras$hour[!str_detect(marche_stras$hour, "[0-9]")] <- NA
marche_stras <- marche_stras %>% 
  fill(hour)

## Create column name
marche_stras <- marche_stras %>% 
  mutate(name = marche_stras$.)
marche_stras$name[marche_stras$name %in% c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi")] <- NA
marche_stras$name[str_detect(marche_stras$name, "De [0-9]")] <- NA
### Trim white space
marche_stras <- marche_stras %>% 
  mutate(name = str_trim(name),
         name = str_replace(name, "Livres \\(rue des Hallebardes", "Rue des Hallebardes \\(livres\\)"),
         name = str_replace(name, "place Kléber\\)", "Place Kléber \\(livres\\)"),
         name = str_replace(name, "Fleurs \\(rue des Grandes Arcades\\)", "Rue des Grandes Arcades \\(fleurs\\)"),
         name = str_replace(name, "Producteurs \\(rue de la Douane\\)", "Rue de la Douane \\(producteurs\\)"))

## Drop duplicates 
marche_stras <- marche_stras %>% 
  drop_na(name) %>% 
  select(name, day, hour)

## Create column location (for nominatim)
marche_stras <- marche_stras %>% 
  mutate(location = name,
         location = str_replace_all(location, "\\(.*\\)", ""),
         location = str_replace_all(location, "Place de l'Esplanade", "Place de l'Esplanade, Strasbourg"),
         location = str_replace_all(location, " à la", ","),
         location = str_replace_all(location, " à", ","),
         location = str_replace_all(location, " au", ","),
         location = str_replace_all(location, "é, Neudorf", "e Neudorf"),
         location = str_replace_all(location, "Cité de l'Ill, Rue de la Doller, Robertsau", "Rue de la Doller, Strasbourg"),
         location = str_replace_all(location, "Place de Bordeaux", "Place de Bordeaux, Strasbourg"),
         location = str_replace_all(location, "Rue des Hallebardes", "Rue des Hallebardes, Strasbourg"),
         location = str_replace_all(location, "Rue des Grandes Arcades", "Rue des Grandes Arcades, Strasbourg"),
         location = str_replace_all(location, "Place Kléber", "Place Kleber"),
         location = str_replace_all(location, "Rue de la Douane", "Rue de la Douane, Strasbourg"),
         location = str_replace_all(location, "Brocante", "Place de la Grande Boucherie, Strasbourg"),
         location = str_replace_all(location, "Place du Maillon, Maille Irène et place André Maurois, Hautepierre", "Place Andre Maurois, Hautepierre"),
         location = str_replace_all(location, "Rue du Faubourg National - Installé provisoirement Place Hans Jean Arp( |)", "Place Hans-Jean Arp, Strasbourg"),
         location = str_replace_all(location, marche_stras$name[24], "Place Hans-Jean Arp, Strasbourg"),
         location = str_replace_all(location, "Marché de la Montagne et de l'Artisanat", "Place du Marche aux Poissons, Strasbourg"),
         location = str_replace_all(location, "Rue Watteau, l'Elsau", "Rue Watteau, Strasbourg"),
         location = str_replace_all(location, "Place de l'Ile de France, Meinau-Canardière", "Place de l'Ile de France"),
         location = str_replace_all(location, "Allée", "Allee"),
         location = str_replace_all(location, "Rue Virgile, Koenigshoffen-Hohberg", "Rue Virgile"))


# Get coordinates of locations
df_raw <- tibble()

for (loc in marche_stras$location) {
  raw <- osm_search(loc, 
                    limit = 1, 
                    key = "Dou2eQtrhYMP1hsuTSLr3mXuGbiJ66sy") 
  df_raw <- bind_rows(df_raw, raw)
}

# Prepare data for merging
df_raw <- df_raw %>% 
  select(display_name, lat, lon) %>% 
  separate(display_name, into = "key", ",", extra = "drop") %>% 
  mutate(key = str_replace(key, "Kléber", "Kleber "),
         key = str_replace(key, "Marché-Neudorf", "Marche Neudorf"),
         key = str_replace(key, "é", "e"))
marche_stras <- marche_stras %>% 
  separate(location, into = "key", ",", extra = "drop")

# Merge data
marche_stras_full <- left_join(marche_stras, df_raw, by = "key") %>% 
  distinct() %>% 
  select(-key)

# Add location for "Place du Corps de Garde" manually (not found by nominatim)
marche_stras_full$lat[is.na(marche_stras_full$lat)] <- 48.6000378
marche_stras_full$lon[is.na(marche_stras_full$lon)] <- 7.7801695

# Create column indicating food market
marche_stras_full <- marche_stras_full %>% 
  mutate(food = ifelse(str_detect(name, "livres|fleurs|Brocante"), FALSE, TRUE))

# Create column for markers
marche_stras_full <- marche_stras_full %>% 
  mutate(type = factor(
    case_when(str_detect(name, "livres") ~ "book",
              str_detect(name, "fleurs") ~ "flower",
              str_detect(name, "Brocante") ~ "brocante",
              str_detect(name, ".*") ~ "basket"),
    c("book", "flower", "brocante", "basket")))

# Save data set
write_rds(marche_stras_full, "10_marches_strasbourg/marche_stras.rds")