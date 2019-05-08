# -----------------------------------------------------------------------
# Project: Analyse cinemas in Strasbourg
# Begin: 27/04/2019
# End: xxx
# -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(readxl)
library(nominatim)
library(leaflet)
library(gridExtra)

# Create basic leaflet map
## Get coordinates of cinemas
cinema_map_raw <- osm_search("cinema strasbourg", limit=20, key = "Dou2eQtrhYMP1hsuTSLr3mXuGbiJ66sy")
## Tidy data
cinema_map <- cinema_map_raw %>% 
  transmute(name = unlist(map(str_split(cinema_map_raw$display_name, ","), pluck(1))),
            lat = lat,
            lon = lon)

## Define map
map <- leaflet(cinema_map) %>%
  addTiles() %>%
  addMarkers(
    lng = ~lon,
    lat = ~lat,
    popup = ~name
  )  %>%
  # Set view to Strasbourg city
  setView(lng = 7.7534101, lat = 48.5812226, zoom = 14) 
map



# Define functions

## Load sheets from Excel file
load_sheet <- function(sheet_name, i){
  path <- "/home/david/Documents/GitHub/chodera.github.io/assets/projects/6. Cinemas in Strasbourg/"
  sheet_name <- read_excel(str_c(path, "exploitation - données par commune.xlsx"), sheet = sheet_name, skip = i)
}

## Tidy sheets
clean_sheet <- function(sheet){
  # Get name of data frame
  df_name <- deparse(substitute(sheet))
  
  sheet <- sheet %>% 
    # Rename columns
    rename(code = "...1",
           city = "...2") %>% 
    # Filter for Strasbourg
    filter(city == "Strasbourg") %>% 
    gather(c("1960":"2017"), key = "year", value = "col") %>% 
    select(-c(code, city)) %>% 
    mutate(year = as.numeric(year))
  
  # Set correct column name
  names(sheet)[names(sheet) == "col"] <- df_name
  
  # Assign changed data frame to original data frame
  assign(df_name, sheet, envir = .GlobalEnv)
}

## Set correct column names
set_col <- function(list, names){
  for (i in 1:length(list)){
    names(list[[i]])[[2]] <- sheet_names[[i]]
  }
  return(list)
}


# Load data
number_of_cinemas <- load_sheet("établissements", 6)
screens <- load_sheet("écrans", 6)
seats <- load_sheet("fauteuils", 6)
multiplex <- load_sheet("multiplexes", 6)
showing <- load_sheet("séances", 6)
entries <- load_sheet("entrées", 6)
revenue <- load_sheet("recettes", 6)
avg_rev_per_entry <- load_sheet("RME", 6)
frequentation_index <- load_sheet("indice de fréquentation", 8)
occupancy_rate <- load_sheet("taux d'occupation des fauteuils", 6)

# Assign sheet_names to a vector
sheet_names <- c("number_of_cinemas",
                 "screens",
                 "seats",
                 "multiplex",
                 "showing",
                 "entries",
                 "revenue",
                 "avg_rev_per_entry",
                 "frequentation_index",
                 "occupancy_rate")

# Assign data frames to a list
data <- list(number_of_cinemas,
             screens,
             seats,
             multiplex,
             showing,
             entries,
             revenue,
             avg_rev_per_entry,
             frequentation_index,
             occupancy_rate)
names(data) <- sheet_names

# Clean data
clean_data <- map(data, clean_sheet) %>% 
  set_col(sheet_names)

# Join data frames
cinemas_stras <- left_join(clean_data$number_of_cinemas, clean_data$screens, by = "year") %>% 
  left_join(., clean_data$seats, by = "year") %>% 
  left_join(., clean_data$multiplex, by = "year") %>% 
  left_join(., clean_data$showing, by = "year") %>% 
  left_join(., clean_data$entries, by = "year") %>% 
  left_join(., clean_data$revenue, by = "year") %>% 
  left_join(., clean_data$avg_rev_per_entry, by = "year") %>% 
  left_join(., clean_data$frequentation_index, by = "year") %>% 
  left_join(., clean_data$occupancy_rate, by = "year") 

# Remove separate data frames
rm(list = c(sheet_names, "clean_data", "data"))

# Save data
write_csv(cinemas_stras, "cinemas_stras.csv")

# Plot data
## Number of cinemas
ggplot(data = cinemas_stras) +
  aes(x = year, y = number_of_cinemas, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(breaks = c(0:9)) +
  expand_limits(y = 1) +
  labs(x = "Year",
       y = "Number of cinemas") +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
      panel.background = element_rect(fill = NA),
      panel.grid.major.x = element_line(colour = "lightgray"),
      panel.grid.major.y = element_line(colour = "lightgray"),
      axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(colour="black"))

## Number of seats
seats <- ggplot(data = cinemas_stras) +
  aes(x = year, y = seats, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(1, 10000)) +
  expand_limits(y = 1) +
  labs(x = "Year",
       y = "Number of seats") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 3800, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black")) 

## Number of screens
screens <- ggplot(data = cinemas_stras) +
  aes(x = year, y = screens, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(1, 50)) +
  expand_limits(y = 1) +
  labs(x = "Year",
       y = "Number of screens") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 25, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

## Number of movies shown
movies <- ggplot(data = cinemas_stras) +
  aes(x = year, y = showing, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(1, 100000)) +
  expand_limits(y = 1) +
  labs(x = "Year",
       y = "Number of movies shown") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 40000, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

## Number of entries
entries <- ggplot(data = cinemas_stras) +
  aes(x = year, y = entries, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(1, 3000000)) +
  expand_limits(y = 1) +
  labs(x = "Year",
       y = "Number of entries") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 1000000, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

## Revenue
revenue <- ggplot(data = cinemas_stras) +
  aes(x = year, y = revenue, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(0, 15000000)) +
  #expand_limits(y = 1) +
  labs(x = "Year",
       y = "Revenue") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 6000000, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

## Frequentation index
freq <- ggplot(data = cinemas_stras) +
  aes(x = year, y = frequentation_index, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(0, 10)) +
  #expand_limits(y = 1) +
  labs(x = "Year",
       y = "Frequentation index") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 4, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

## Occupancy rate
occ <- ggplot(data = cinemas_stras) +
  aes(x = year, y = occupancy_rate, group = 1) +
  geom_line() +
  scale_x_continuous(breaks = c(1992:2017)[seq(1, length(c(1992:2017)), 2)], limits = c(1992, 2017)) +
  scale_y_continuous(limits = c(0, 22)) +
  #expand_limits(y = 1) +
  labs(x = "Year",
       y = "Occupancy rate") +
  geom_vline(xintercept = 1999.5, colour = "red") +
  annotate("text", x = 2005, y = 9, label = "Opening of multiplex cinema", family="Merriweather", size = 3) +
  theme_bw() +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "lightgray"),
        panel.grid.major.y = element_line(colour = "lightgray"),
        axis.text.x = element_text(colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(colour="black"))

# Plot graphs
grid.arrange(seats, screens, movies, entries, revenue, freq, occ, ncol = 2)

# Save plots
save(seats, screens, movies, entries, revenue, freq, occ, file = "cinemas_plots.RData")
