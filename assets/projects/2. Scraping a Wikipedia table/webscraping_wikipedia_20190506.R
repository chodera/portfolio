# Loading packages
library(rvest)
library(tidyverse)

# Specify the url for desired website to be scraped
url <- 'https://fr.wikipedia.org/wiki/Liste_des_communes_du_Bas-Rhin'

# Read the HTML code from the website
webpage <- read_html(url)

# Use CSS selectors to scrape the table
table <- html_nodes(webpage,'table.wikitable')

# Converting the table to a data frame
table <- html_table(table, header = TRUE)

bas_rhin <- table %>% 
  bind_rows() %>% 
  as_tibble()

# Look at the data
names(bas_rhin)

# Nom
head(bas_rhin$Nom)
# Data-Preprocessing: remove additional information
bas_rhin %>% 
  filter(str_detect(Nom, "\\(")) %>% 
  select(Nom)

bas_rhin <- bas_rhin %>% 
  mutate(Nom = str_replace_all(Nom, "\\(préfecture\\)", "")) %>% 
  rename(nom = Nom)

# CodeInsee
head(bas_rhin$CodeInsee)
# Data-Preprocessing: converting to numerical
bas_rhin <- bas_rhin %>% 
  mutate(CodeInsee = as.numeric(CodeInsee)) %>% 
  rename(code_insee = CodeInsee)

# Code postal
head(bas_rhin$`Code postal`)
# Data-preprocessing: split data
# Problem: some have more than one value and got concatenated, Strasbourg even has three
# Strategy: split into several variables
bas_rhin <- bas_rhin %>% 
  mutate(code_postal_1 = as.numeric(str_sub(`Code postal`, 1, 5)),
         code_postal_2 = as.numeric(str_sub(`Code postal`, 6, 10)),
         code_postal_3 = as.numeric(str_sub(`Code postal`, 11, 15))) %>% 
  select(nom, code_postal_1, code_postal_2, code_postal_3, everything(), -`Code postal`)
  
# Arrondissement
head(bas_rhin$Arrondissement)
bas_rhin <- bas_rhin %>% 
  rename(arrondissement = Arrondissement)

# Canton
head(bas_rhin$Canton)
bas_rhin <- bas_rhin %>% 
  rename(canton = Canton)

# Intercommunalité
head(bas_rhin$Intercommunalité)
bas_rhin <- bas_rhin %>% 
  rename(intercommunalité = Intercommunalité)

# Superficie
head(bas_rhin$`Superficie(km2)`)
# Data-Preprocessing: converting to numerical
bas_rhin <- bas_rhin %>% 
  mutate(`Superficie(km2)` = str_replace_all(`Superficie(km2)`, ",", "."),
         `Superficie(km2)` = as.numeric(`Superficie(km2)`)) %>% 
  rename(superficie = `Superficie(km2)`)

# Population
head(bas_rhin$`Population(dernière pop. légale)`)
# Data-Preprocessing: removing (2016) and (2015)
bas_rhin <- bas_rhin %>% 
  mutate(`Population(dernière pop. légale)` = str_replace_all(`Population(dernière pop. légale)`, " \\(2016\\)|\\(2015\\)", ""),
         `Population(dernière pop. légale)` = str_replace_all(`Population(dernière pop. légale)`, "\\p{WHITE_SPACE}", ""),
         `Population(dernière pop. légale)` = as.numeric(`Population(dernière pop. légale)`)) %>%
  rename(population = `Population(dernière pop. légale)`)

# Densité
head(bas_rhin$`Densité(hab./km2)`) 
# Data-Preprocessing: removing white space and converting to numeric
bas_rhin <- bas_rhin %>%   
  mutate(`Densité(hab./km2)` = str_replace_all(`Densité(hab./km2)`, "\\p{WHITE_SPACE}", ""),
         `Densité(hab./km2)` = as.numeric(`Densité(hab./km2)`)) %>%
  rename(densite = `Densité(hab./km2)`)

# Delete last column "Modifier"
bas_rhin <- bas_rhin %>% 
  select(-Modifier)

# Remove the line with information on the whole département
bas_rhin <- bas_rhin %>%
  filter(nom != "Bas-Rhin")

# Save data set
write.csv(bas_rhin, "bas_rhin.csv")

# Analyze data
# Area
bas_rhin %>% 
  arrange(desc(superficie)) %>% 
  select(nom, superficie) %>% 
  head(n = 10)
# The biggest city by area is Hagenau, an online research reveals that 
# a big part of the city consists of an old forest

# Population
bas_rhin %>% 
  arrange(desc(population)) %>% 
  select(nom, population) %>% 
  head(n = 10)

# Density
bas_rhin %>% 
  arrange(desc(densite)) %>% 
  select(nom, densite) %>% 
  head(n = 10)
