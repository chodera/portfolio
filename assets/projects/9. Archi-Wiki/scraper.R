# Load packages
library(tidyverse)
library(rvest)

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
i <- 1 # Set counter (for counting pages)

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
  
  # Increment counter
  i = i + 1
  
  # Print page number
  print(str_c("Scraping page ", i))

}

# Clean links
clean_links <- full_data %>%
  filter(str_detect(link_texts, "^Adresse:")) %>%
  pull(links)

# Set final urls
urls <- str_c("https://www.archi-wiki.org", clean_links)


##########################################
# Now that we have the urls, we can loop over them and get specific elements

# Set url
url_test <- "https://www.archi-wiki.org/Adresse:%22Bijouterie_Minner,_135_ans_d%27excellence%22_(Colmar)"

# Load html
page <- read_html(url_test)

# Get date from page
date_full <- page %>% 
  html_nodes(".infobox-actu") %>% 
  html_text() %>% 
  .[1]
 
i = 1
pb   <- txtProgressBar(1, 100000, style=3)

for (url in urls) {
  
  # Load html
  page <- read_html(url)
  
  # Get date
  date <- page %>% 
    html_nodes(".infobox-actu") %>% 
    html_text() %>% 
    .[1]
  
  date_full <- c(date_full, date)
  
  # Increment counter
  i = i + 1
  setTxtProgressBar(pb, i)
  #print(str_c(round((i/length(urls) * 100), 2), " %"))
}

# Put together to a tibble
full_data <- tibble(links, link_texts)
date <- tibble(date_full) %>% 
  count(is.na(date_full))
