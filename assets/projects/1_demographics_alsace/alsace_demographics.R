# Load packages
library(tidyverse)
library(readxl)
# library(maptools)
# library(classInt)	
# library(RColorBrewer)	
# library(extrafont)
# library(sf)

# Load shape file
france_map <- sf::st_read("C:/Program Files/R/R-3.5.3/library/shiny/examples/france/data/carte/n_com_fla_000.shp")

# france_map <- sf::st_read("/home/david/Documents/GitHub/chodera.github.io/assets/projects/1. Demographics in Alsace/data/communes-20190101-shp/communes-20190101.shp")
# 
# france_map2 <- france_map %>% 
#   filter(str_detect(as.character(insee), "^(67|68)")) %>% 
#   select(com = insee, geometry) 
# 
# france_map$com
# 
# levels(france_map$com)

france_map %>%
  filter(str_detect(levels(insee), "^(67|68)"))

# Select only Alsace
alsace_map <- france_map %>%
  filter(code_regio == 42) %>% 
  select(insee_comm, geometry) %>% 
  rename(com = insee_comm)

# Load population data
france_pop <- read_excel("/home/david/Documents/GitHub/chodera.github.io/assets/projects/1. Demographics in Alsace/data/base_cc_comparateur.xls", 
                         sheet = "COM", skip = 5)

# Select only data for Alsace
alsace_pop <- france_pop %>% 
  filter(str_detect(as.character(CODGEO), "^6(7|8)")) %>%
  select(CODGEO, P15_POP, SUPERF, NAISD17, DECESD17, P15_LOGVAC, P15_CHOM1564, P15_ACT1564) %>% 
  rename(com = CODGEO, pop = P15_POP, superf = SUPERF, naiss = NAISD17, deces = DECESD17, logvac = P15_LOGVAC, chom = P15_CHOM1564, activ = P15_ACT1564) %>% 
  mutate(densite = pop/superf, 
         chomage = 100 * chom/activ,
         natalite = 1000 * naiss/pop, 
         mortalite = 1000 * deces/pop,
         logements_vacants = 1000 * logvac/pop) %>% 
  select(com, densite, chomage, natalite, mortalite, logements_vacants)

View(alsace_pop)

# Merge data sets
alsace <- left_join(alsace_map, alsace_pop, by = "com") 

# Check missing data
mis <- alsace %>%
  filter(is.na(pop)) %>% 
  View()
# It is actually old communes which do not exist anymore.

# Save data set
saveRDS(alsace, file = "C:/Program Files/R/R-3.5.3/library/shiny/examples/france/data/alsace.rds")

########################


alsace <- read_rds("~/Documents/1. Data Science/R/alsace-app/data/alsace.rds")

# Plot map
## Define range and colors 
distr	<- classIntervals(alsace$dens,10,style="quantile")$brks
display.brewer.all()	#	affiche	les	palettes	disponibles	
couleurs<- rev(brewer.pal(10,"Spectral"))	#	palette Spectral
colMap <-couleurs[(findInterval(alsace$densite, distr,	all.inside=TRUE))]	

## saving defaults in `op`
op <- par() 

## set font family 
# windowsFonts(
#   A=windowsFont("Merriweather")
# )
par(family = "Merriweather")

plot(select(alsace, densite), col=colMap, main = "")	

legend("bottomleft", 
       legend=leglabs(round(distr,2),	
                      over=">",	
                      under="<"),	
       fill=couleurs, bty = "n")	

ggplot(alsace, col=colMap, main = "Testtest")	

## reset plotting parameters
par(op)
