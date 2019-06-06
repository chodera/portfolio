# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)


# Define urls -------------------------------------------------------------

# Base
base <- "https://github.com/muc-fluechtlingsrat/bamf-asylgeschaeftsstatistik/raw/master/raw/"

# Suffix
## years
doc_years <- rep(2015:2019, each = 12)

## months
doc_months <- seq(1, 12) %>% 
  str_pad(2, "left", pad = "0")

## combine to suffix
suffix <- str_c(doc_years, "/", doc_years, doc_months, ".csv")
suffix <- suffix[-c(53:length(doc_years))] # for 2019 only data until 042019 available, so drop other months

# Combine base and suffix to full urls
urls <- str_c(base, suffix)


# Check data  -------------------------------------------------------------

# Let's see how the data looks like for one data set
csv_file <- read_csv(urls[1])
glimpse(csv_file)

# We are actually only interested in the sum of the column `ASYLANTRAEGE insgesamt` and the month
csv_file %>% 
  summarise(n = sum(`ASYLANTRAEGE insgesamt`))
csv_file %>% 
  distinct(YEAR_MONTH)

# Let's get this data for all csv files  


# Get data ----------------------------------------------------------------

i <- 1
dates <- vector()
n <- vector()

for (url in urls) {
  csv_file <- read_csv(urls[i])
  
  date <- csv_file %>% 
    distinct(YEAR_MONTH)
  
  n_month <- csv_file %>% 
    summarise(n_month = sum(`ASYLANTRAEGE insgesamt`))
  
  dates <- c(dates, date)
  n <- c(n, n_month)
  i <-  i + 1
}


# Clean data --------------------------------------------------------------

# Flatten lists
dates <- unlist(dates)
n <- unlist(n)

# Enframe lists
dates <- enframe(dates) %>% 
  select(date = value)
n <- enframe(n) %>% 
  select(n = value)

# Combine tibbles
df <- bind_cols(dates, n)


# Save data set -----------------------------------------------------------

write_csv(df, "asylmonatszahlen.csv")
