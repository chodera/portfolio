# -----------------------------------------------------------------------
# Project: Analysis most popular movies in France
# Begin: 27/04/2019
# End: xxx
# -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(readxl)

# Load data
path <- "/home/david/Documents/GitHub/chodera.github.io/assets/projects/6. Cinemas in Strasbourg/"
films <- read_excel(str_c(path, "Meilleures audiences en salles depuis 1945.xlsx"), skip = 4)

# Check data
# View(films)

# Delete last 3 lines (meta info)
films <- films %>% 
  slice(1:200)

# Clean data
films <- films %>% 
  rename(rank = rang,
         title = titre,
         director = réalisateur,
         country = nationalité1,
         entries = `entrées (millions)2`)

# Descriptive statistics
## Summarize films by country

### Some countries were produced in several countries:
### the movies are assigned to the first mentioned country, assuming that this was the main production country
films <- films %>% 
  mutate(country_main = str_sub(country, 1, 2),
         country_main = str_replace(country_main, "US", "United States"),
         country_main = str_replace(country_main, "FR", "France"),
         country_main = str_replace(country_main, "GB", "Great Britain"),
         country_main = str_replace(country_main, "IT", "Italy"),
         country_main = str_replace(country_main, "AT", "Austria"),
         country_main = str_replace(country_main, "NZ", "New Zealand"),
         country_main = str_replace(country_main, "AU", "Australia"),
         country_main = str_replace(country_main, "RU", "Russia"),
         country_main = str_replace(country_main, "ZA", "South Africa"))

countries <- films %>% 
  group_by(country_main) %>% 
  summarise(movies_per_country = n(), total_entry = sum(entries)) %>% 
  arrange(desc(movies_per_country))
countries

## TO DO: check for each decade 

## Summarize films by director
directors <- films %>% 
  group_by(director) %>% 
  summarise(total_entry = sum(entries), number_of_movies = n()) %>% 
  mutate(avg_entry = total_entry / number_of_movies) %>% 
  arrange(desc(total_entry))

## Plot 10 most succesful directors 
directors %>% 
  arrange(desc(total_entry)) %>% 
  slice(1:10) %>% 
  arrange(total_entry) %>% 
  mutate(director = factor(director, director),
         number = str_c(number_of_movies, " movies")) %>% 
  ggplot() +
  aes(x = director, y = total_entry) +
  geom_bar(stat = "identity", width=0.6, fill = "grey") +
  coord_flip() +
  geom_text(aes(label = number), hjust = 1.2, family="Merriweather", size = 3) +
  theme(text=element_text(family="Merriweather"),
        panel.background = element_rect(fill = NA),
        panel.grid.major.x = element_line(colour = "grey50"),
        axis.ticks.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_text(colour="black"),
        axis.text.y = element_text(colour="black"),
        legend.position="none")  + 
        scale_y_continuous(
          breaks = c(0, 20000000, 40000000, 60000000),
          label = c("0", "20 mln", "40 mln", "60 mln")
        ) 
