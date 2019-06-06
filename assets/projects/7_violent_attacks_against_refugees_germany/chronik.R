# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(pdftools)

# Load data
## Scraped data from the chronik
df <- read_csv("7_violent_attacks_against_refugees_germany/data/mut_gegen_rechte_gewalt.csv")
## Population by Bundesland (for standardization) (Source: https://www-genesis.destatis.de/genesis/online)
pop <- read_csv2("7_violent_attacks_against_refugees_germany/data/12411-0010.csv", 
                skip = 6, 
                col_names = FALSE, 
                trim_ws = TRUE,
                n_max = 16,
                locale = locale(encoding = "Windows-1252"))
## Official statistics on numbers of refugees (Source: https://github.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik)
statistik <- read_csv("7_violent_attacks_against_refugees_germany/data/asylmonatszahlen.csv")


# Data wrangling ----------------------------------------------------------

# Data from Chronik
df <- df %>% 
  # Turn date into date object
  mutate(date = dmy(date)) 
attr(df, "spec") <- NULL # remove attribute

# Population data
pop <- pop %>%
  rename(bundesland = X1, population = X2)
attr(pop, "spec") <- NULL # remove attribute

# Data on monthly refugee numbers
statistik <- statistik %>% 
  # Turn date into date object
  mutate(date = str_c(statistik$date, "-01"),
         date = ymd(date)) 
attr(statistik, "spec") <- NULL # remove attribute


# EDA ---------------------------------------------------------------------

# Frequency plot: Attacks per month
ggplot(df, aes(date)) + 
  geom_freqpoly(binwidth = 30) + # 30 days 
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", angle = 45, hjust = 1)) +
  labs(x = "", 
       y = "Number of attacks", 
       title = "Attacks on refugees per month in Germany") + 
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")

###
ggplot(statistik, aes(x = date, y = n, group = 1)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "black", angle = 45, hjust = 1)) +
  labs(x = "", 
       y = "Number of asylum requests", 
       title = "Monthly asylum requests in Germany") +  
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")


# Attacks by Bundesland
## Count
df_bundesland <- df %>% 
  group_by(bundesland) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(bundesland = factor(bundesland, levels = bundesland[order(n)]))

df_bundesland %>% 
  arrange(desc(n))

## Histogram
ggplot(df_bundesland) +
  geom_bar(aes(x = bundesland, y = n), stat = "identity") +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank()) +
  xlab("") +
  ylab("") 


# Attacks by Bundesland (normalized by population)
## Add population data
df_bundesland <- left_join(df_bundesland, pop, by = "bundesland")

## Compute standardized attack rate (100000 attacks per person)
df_bundesland <- df_bundesland %>% 
  mutate(n_std = n * 100000 / population)

## Show data frame
df_bundesland %>% 
  arrange(desc(n_std))

## Histogram
df_bundesland %>% 
  mutate(bundesland = factor(bundesland, levels = bundesland[order(n_std)])) %>% 
  ggplot() +
  geom_bar(aes(x = bundesland, y = n_std), stat = "identity") +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank()) +
  xlab("") +
  ylab("") 


# Histogram: Attacks by category
## Count
df_category <- df %>% 
  group_by(category) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(category = factor(category, levels = category[order(n)]))

df_category %>% 
  arrange(desc(n))

## Histogram
ggplot(df_category) +
  geom_bar(aes(x = category, y = n), stat = "identity") +
  coord_flip() +
  theme_light() +
  theme(axis.text.x = element_text(color = "black"), 
        axis.text.y = element_text(color = "black"),
        axis.ticks.y = element_blank()) +
  xlab("") +
  ylab("") 


# Casualties
df %>% 
  count(!is.na(casualties))
## Only 611 observations contain information on casualties.

## DO IT IN PYTHON CAUSE CLEANING IS ALREADY DONE THERE.


# Sources
df %>% 
  count(!is.na(source))
## There is missing a source for around 10% of the events.

df_source <- df %>% 
  group_by(source) %>% 
  count() %>% 
  arrange(desc(n))
## By far, most sources are related to "Kleine Anfragen" in the Bundestag.

## Create a variable categorizing the sources
df_source <- df_source %>% 
  mutate(source_category = case_when(str_detect(source, "Anfrage|Bundesregierung") ~ "Regierung",
                                     str_detect(source, "[Pp]olizei") ~ "Polizei",
                                     str_detect(source, "Anfrage|Bundesregierung|[Pp]olizei", negate = TRUE) ~ "Other"))

df_source %>% 
  group_by(source_category) %>% 
  summarise(n = sum(n)) %>% 
  arrange(desc(n))
