# Unequal political participation in Europe

# Load packages
library(tidyverse)
library(haven)
library(sf)
library(leaflet)


# 1. Import and prepare data ----------------------------------------------

# Load survey and macro data (Stata file)
ess <- read_dta("8_unequal_political_participation_europe/data/full_data.dta")

## Extract relevant data by NUTS 1 region
ess_by_region <- ess %>%
  drop_na() %>% 
  group_by(id_r) %>% 
  summarise(vote = round(mean(vote * 100), 2),
            demo = round(mean(demo * 100), 2),
            workparty = round(mean(workparty * 100), 2),
            boycott = round(mean(boycott * 100), 2)) %>% 
  drop_na()

# Load shape file
## Source: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
nuts1 <- read_sf("8_unequal_political_participation_europe/data/NUTS_RG_60M_2013_4326_LEVL_1.shp")

nuts1 <- nuts1 %>% 
  # Drop irrelavant countries
  filter(!CNTR_CODE %in% c("BG", "CY", "DK", "EL", "LV", "LI", "ME", "MK", "MA", "RO", "TR", "HR", "SK")) %>% 
  select(id_r = NUTS_ID,
         id_c = CNTR_CODE)

# Combine both data sets
ess_full <- left_join(nuts1, ess_by_region, by = "id_r") 

# Save data set
write_rds(ess_full, "ess_plus_nuts.rds")

# 2. Visualize data -------------------------------------------------------

# Interactive maps

## Set base map
base_map <- leaflet(ess_full) %>%
  setView(10, 51, 3) %>%
  addTiles()

# 1. Vote
## Define colors
bins <- c(seq(50, 100, by = 10)) 
pal <- colorBin("RdYlGn", domain = ess_full$vote, bins = bins)

## Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>Vote share: %g<br/>",
  ess_full$id_r, ess_full$vote 
) %>% lapply(htmltools::HTML)

## Set map
vote_map <- base_map %>% 
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

# 2. Demonstrating
## Define colors
bins <- c(seq(0, 30, by = 5)) 
pal <- colorBin("RdYlGn", domain = ess_full$demo, bins = bins)

## Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>%g&#37;  demonstrating<br/>",
  ess_full$id_r, ess_full$demo 
) %>% lapply(htmltools::HTML)

## Set map
demo_map <- base_map %>% 
  addPolygons(
    fillColor = ~pal(demo),
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
  addLegend(pal = pal, values = ~demo, opacity = 0.7, title = NULL,
            position = "bottomright")

# 3. Work for party or political action group
## Define colors
bins <- c(seq(0, 15, by = 3)) 
pal <- colorBin("RdYlGn", domain = ess_full$workparty, bins = bins)

## Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>%g&#37; working for party<br/>",
  ess_full$id_r, ess_full$workparty 
) %>% lapply(htmltools::HTML)

## Set map
work_map <- base_map %>% 
  addPolygons(
    fillColor = ~pal(workparty),
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
  addLegend(pal = pal, values = ~workparty, opacity = 0.7, title = NULL,
            position = "bottomright")

# 4. Boycotting products
## Define colors
bins <- c(seq(0, 60, by = 10)) 
pal <- colorBin("RdYlGn", domain = ess_full$boycott, bins = bins)

## Define labels
labels <- sprintf(
  "<strong>NUTS code: %s</strong><br/>%g&#37; boycotting products<br/>",
  ess_full$id_r, round(ess_full$boycott, 2) 
) %>% lapply(htmltools::HTML)

## Set map
boycott_map <- base_map %>% 
  addPolygons(
    fillColor = ~pal(boycott),
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
  addLegend(pal = pal, values = ~boycott, opacity = 0.7, title = NULL,
            position = "bottomright")


# Bar plots (by country and European Socioeconomic Classification)
## Prepare data
ess_bar <- ess %>% 
  drop_na() %>% 
  filter(esec == 1 | esec == 9) %>% 
  mutate(id_c = as_factor(id_c),
         vote = as.integer(vote) * 100,
         demo = as.integer(demo) * 100,
         workparty = as.integer(workparty) * 100,
         boycott = as.integer(boycott) * 100,
         esec_top_bottom = as_factor(ifelse(esec == 9, "Highest ESeC", "Lowest ESeC"))) %>% 
  select(id_c, vote, demo, workparty, boycott, esec_top_bottom)
ess_bar$esec_top_bottom <- factor(ess_bar$esec_top_bottom, levels = rev(levels(ess_bar$esec_top_bottom)))

## 1. Vote
vote_bar <- ggplot(ess_bar, aes(x = id_c, y = vote, fill = esec_top_bottom)) + 
  geom_bar(position = "dodge", colour="black", alpha = 0.7, stat = "summary", fun.y = "mean") +
  xlab("") +
  ylab("Participation in voting [%]") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")

## 2. Demo
demo_bar <- ggplot(ess_bar, aes(x = id_c, y = demo, fill = esec_top_bottom)) + 
  geom_bar(position = "dodge", colour="black", alpha = 0.7, stat = "summary", fun.y = "mean") +
  xlab("") +
  ylab("Attending demonstrations [%]") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")

## 3. Work for party
work_bar <- ggplot(ess_bar, aes(x = id_c, y = workparty, fill = esec_top_bottom)) + 
  geom_bar(position = "dodge", colour="black", alpha = 0.7, stat = "summary", fun.y = "mean") +
  xlab("") +
  ylab("Working for political party [%]") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")

## 4. Boycotting products
boycott_bar <- ggplot(ess_bar, aes(x = id_c, y = boycott, fill = esec_top_bottom)) + 
  geom_bar(position = "dodge", colour="black", alpha = 0.7, stat = "summary", fun.y = "mean") +
  xlab("") +
  ylab("Boycotting products [%]") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title = element_blank()) + 
  scale_fill_brewer(palette = "Dark2")

# Save plots
save(vote_bar, demo_bar, work_bar, boycott_bar, file = "unequal_plots.RData")
