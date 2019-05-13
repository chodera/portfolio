# -----------------------------------------------------------------------
# Project: History of buildings in Strasbourg
# Title: Scraping Archi-Wiki
# Begin: 10/05/2019
# End: xxx
# -----------------------------------------------------------------------


# TO DO ADD EMPTY STRINGS IN CASE THERE IS NO LON AND LAT IN FOR LOOP!
# OR PERHAPS EVEN NOT NECESSARY BECAUSE THERE IS THE REFERENTIEL ADRESSE - LOCATION

# Load packages
library(tidyverse)
library(rvest)


# Strategy ----------------------------------------------------------------

# 1. Obtain urls of all buildings indexed on 
# 2. Loop over all urls and scrape desired information


# 1. Obtain urls  ---------------------------------------------------------

# Set first url
url_1 <- "https://www.archi-wiki.org/Sp%C3%A9cial:Toutes_les_pages/Adresse:?from=&to=&namespace=0"

# Load html
page <- read_html(url_1)

# Get all links from page
links <- page %>% 
  html_nodes("a") %>% 
  html_attr('href')

# Get link text
link_texts <- page %>% 
  html_nodes("a") %>% 
  html_text() 

# Put together to a tibble
full_data <- tibble(links, link_texts)

# Get link to next page
next_page <- full_data %>% 
  filter(str_detect(link_texts, "Page suivante")) %>% 
  distinct() %>% 
  pull(links)

# Set url for next page
url_next_page <- str_c("https://www.archi-wiki.org", next_page)

# Now we would set the second url, get all links and texts, find the next page and so on 
# -> we use a while loop to do the work for us

## Set progress bar
i <- 0
pb <- txtProgressBar(0, 38, style=3) 

while (length(next_page) > 0) {
  
  # Load html
  page <- read_html(url_next_page)
  
  # Get all links from page
  links <- page %>% 
    html_nodes("a") %>% 
    html_attr('href')
  
  # Get link text
  link_texts <- page %>% 
    html_nodes("a") %>% 
    html_text() 
  
  # Put together to a tibble
  page <- tibble(links, link_texts)
  
  # Append to full data frame
  full_data <- full_data %>% 
    bind_rows(page)
  
  # Get link to next page
  next_page <- page %>% 
    filter(str_detect(link_texts, "Page suivante")) %>% 
    distinct() %>% 
    pull(links)
  
  # Set url for next page
  url_next_page <- str_c("https://www.archi-wiki.org", next_page)
  
  # Progress bar
  i = i + 1
  setTxtProgressBar(pb, i)
  
}

# Filter ulrs (only containing Strasbourg and starting with Adresse)

# Clean links
df_clean <- full_data %>%
  filter(str_detect(link_texts, "^Adresse:")) %>%
  filter(str_detect(link_texts, "Strasbourg"))

# Pull links and link texts
clean_links <- df_clean %>% 
  pull(links)
link_texts <- df_clean %>% 
  pull(link_texts)

# Set final urls
urls <- str_c("https://www.archi-wiki.org", clean_links)

# Save urls in R object
write_rds(urls, "9_archi-wiki/data/urls.rds")


# 2. Loop over all urls and scrape data -----------------------------------

# # Set url
# url_test <- "https://www.archi-wiki.org/Adresse:Fa%C3%A7ade_nord,_c%C3%B4t%C3%A9_ouest_(Strasbourg)"
# 
# urls <- c("https://www.archi-wiki.org/Adresse:Fa%C3%A7ade_nord,_c%C3%B4t%C3%A9_ouest_(Strasbourg)",
#           "https://www.archi-wiki.org/Adresse:15_rue_du_Sanglier_(Strasbourg)",
#           "https://www.archi-wiki.org/Adresse:15_rue_du_Sanglier_(Strasbourg)")
# 
# # Load html

# Scrape date of construction, structure and coordinates from every page
## Set empty vectors
date_full <- c(character(0))
structure_full <- c(character(0))
lat_full <- c(character(0))
lon_full <- c(character(0))

## Set progress bar
i <- 0
pb <- txtProgressBar(0, length(urls), style=3)

## Loop over all urls 
for (url in urls) {
  
  # Load html
  page <- read_html(url)
  
  # Extract latitude and longitude from leaflet map (first coordinates that occur)
  lat <- page %>% 
    html_nodes("#map_leaflet_1") %>% 
    html_text() %>% 
    str_extract('"lat":\\d+.\\d+') %>% 
    str_extract("\\d+.\\d+")
  lon <- page %>% 
    html_nodes("#map_leaflet_1") %>% 
    html_text() %>% 
    str_extract('"lon":\\d+.\\d+') %>% 
    str_extract("\\d+.\\d+")
  
  # Append to vector
  lat_full <- c(lat_full, lat)
  lon_full <- c(lon_full, lon)
  
  # Extract date of construction and structure from infobox
  # Get children of node "table.infobox"
  children <- page %>% 
    html_nodes("table.infobox") %>% 
    html_children()

  # Set empty string for construction date
  date <- ""
    
  # Loop over all children of node "table.infobox" 
  for (child in children) {
    
    # Get text out of node child
    child_text <- html_text(child)

    # Extract date of construction of building
    if (str_detect(child_text, "Date de construction")) {
      # In case there is more than one date take the first one
      if (nchar(date) == 0) {
        date <- child_text
      } # end if
    } # end if
    
    # Extract structure of building
    else if (str_detect(child_text, "Structure")) {
      structure <- child_text
    } # end else if
    
  } # end inner for-loop

  # Append to vector
  date_full <- c(date_full, date)
  structure_full <- c(structure_full, structure)
  
  # Progress bar
  i = i + 1
  setTxtProgressBar(pb, i)
} # end outer for-loop

# Put together to a tibble
df <- bind_cols(tibble(urls[1:2738]),
                tibble(link_texts[1:2738]),
                tibble(date_full), 
                tibble(structure_full)
                #tibble(lat_full),
                #tibble(lon_full)
                )

## First run went till 2738 -> continue with 2739
length(date_full)/length(urls) * 100 # 26.5 % -> this means we need 4 runs

write_rds(df, "9_archi-wiki/data/archi-wiki_1.rds")
