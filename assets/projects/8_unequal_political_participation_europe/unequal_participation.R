# Unequal political participation in Europe
# This post uses some insights from my master's thesis where I investigated contextual determinants 
# of inequalities in political participation across European regions, especially the role of 
# income inequality. Here I will give a short overview of some descriptive findings. 
# Data for the analysis came from several sources but the main source on individual-level data was
# the European Social Survey (ESS).

# Load packages
library(tidyverse)
library(haven)
library(sf)
library(leaflet)

# Load survey and macro data (Stata file)
ess <- read_dta("//engees-adm/home/recherch/users/dwitowsk/winnt/mes documents/R/Berlin/full_data.dta")

## Extract relevant data by NUTS 1 region
ess_by_region <- ess %>%
  drop_na() %>% 
  group_by(id_r) %>% 
  summarise(vote = mean(vote),
            demo = mean(demo),
            workparty = mean(workparty),
            boycott = mean(boycott),
            r_gini_post = mean(r_gini_post), 
            r_uneq_vote = mean(r_uneq_vote),
            r_uneq_demo = mean(r_uneq_demo),
            r_uneq_work = mean(r_uneq_work),
            r_uneq_boycott = mean(r_uneq_boycott)) %>% 
  drop_na()

# Load shape file
nuts1 <- read_sf("//engees-adm/home/recherch/users/dwitowsk/winnt/mes documents/R/Berlin/map/NUTS_RG_60M_2013_4326_LEVL_1.shp")

nuts1 <- nuts1 %>% 
  # Drop irrelavant countries
  filter(!CNTR_CODE %in% c("BG", "CY", "DK", "EL", "LV", "LI", "ME", "MK", "MA", "RO", "TR", "HR", "SK")) %>% 
  select(id_r = NUTS_ID,
         id_c = CNTR_CODE)


# Combine both data sets
ess_full <- left_join(nuts1, ess_by_region, by = "id_r") 

# Define interactive maps

## 1. Vote
### Define colors
bins <- c(seq(0.5, 1, by = 0.1)) 
pal <- colorBin("RdYlGn", domain = ess_full$vote, bins = bins)

### Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>Vote share: %g<br/>",
  ess_full$id_r, ess_full$vote
) %>% lapply(htmltools::HTML)

### Set map
leaflet(ess_full) %>%
  setView(10, 51, 3) %>%
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(vote),
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
      direction = "auto")) %>% 
  addLegend(pal = pal, values = ~vote, opacity = 0.7, title = NULL,
            position = "bottomright")




## 1. Income inequality
### Define colors
bins <- c(seq(0.2, 0.4, by = 0.02)) 
pal <- colorBin("RdYlGn", domain = ess_full$r_gini_post, bins = bins, reverse = TRUE)

### Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>Gini index: %g<br/>",
  ess_full$id_r, ess_full$r_gini_post
) %>% lapply(htmltools::HTML)

### Set map
leaflet(ess_full) %>%
  setView(10, 51, 3) %>%
  addTiles() %>% 
  addPolygons(
    fillColor = ~pal(r_gini_post),
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
      direction = "auto")) %>% 
  addLegend(pal = pal, values = ~r_gini_post, opacity = 0.7, title = NULL,
            position = "bottomright")

# Bar plots (by country and )
ggplot(data = ess) +
  aes(x = factor(vote)) %>% 
  geom_bar()
  
