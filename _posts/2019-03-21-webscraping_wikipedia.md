---
layout: post
title: "Scraping a Wikipedia table"
output:
  html_document:
    df_print: paged
image: assets/images/wiki.png
normal: true
logo: r
---

I tried out the rvest package by scraping a Wikipedia table about [communities in the Département "Bas-Rhin" in the region of Alsace in France](https://fr.wikipedia.org/wiki/Liste_des_communes_du_Bas-Rhin){:target="_blank"}. First thing to do is to load the necessary packages: rvest for scraping the website, tidyverse to clean the scraped data.

{% highlight r %}
library(rvest)
library(tidyverse)
{% endhighlight %}

Scraping a webpage is surprisingly easy with the convenient rvest package. You only need little knowledge about CSS selectors to access the information you are interested in, in this case the whole table which is converted into a data frame.


{% highlight r %}
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
{% endhighlight %}

The resulting data frame consists of 514 observations and 10 variables.


{% highlight text %}
## # A tibble: 514 x 10
##    Nom   CodeInsee `Code postal` Arrondissement Canton Intercommunalité
##    <chr> <chr>             <dbl> <chr>          <chr>  <chr>           
##  1 Stra… 67482           6.70e14 Strasbourg     Stras… Eurométropole d…
##  2 Ache… 67001           6.72e 4 Strasbourg     Lingo… Eurométropole d…
##  3 Adam… 67002           6.73e 4 Saverne        Ingwi… CC de l'Alsace …
##  4 Albé  67003           6.72e 4 Sélestat-Erst… Mutzig CC de la Vallée…
##  5 Alte… 67005           6.73e 4 Saverne        Bouxw… CC du Pays de l…
##  6 Alte… 67006           6.75e 4 Saverne        Saver… CC du Pays de S…
##  7 Alto… 67008           6.71e 4 Molsheim       Molsh… CC de la région…
##  8 Altw… 67009           6.73e 4 Saverne        Ingwi… CC de l'Alsace …
##  9 Andl… 67010           6.71e 4 Sélestat-Erst… Obern… CC du Pays de B…
## 10 Arto… 67011           6.74e 4 Sélestat-Erst… Séles… CC de Marckolsh…
## # … with 504 more rows, and 4 more variables: `Superficie(km2)` <chr>,
## #   `Population(dernière pop. légale)` <chr>, `Densité(hab./km2)` <chr>,
## #   Modifier <lgl>
{% endhighlight %}

A first glance reveals that there are some problems with the data, which is typical for freshly scraped data. The following section deals with all the data preprocessing, for each variable one by one.


{% highlight r %}
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

{% endhighlight %}

A glimpse on the clean data set (which can be downloaded [here]({{ site.url }}/assets/data/bas_rhin.csv)):

{% highlight text %}
## # A tibble: 514 x 11
##    nom   code_postal_1 code_postal_2 code_postal_3 code_insee
##    <chr> <chr>         <chr>         <chr>              <dbl>
##  1 Stra… 67000         67100         67200              67482
##  2 Ache… 67204         ""            ""                 67001
##  3 Adam… 67320         ""            ""                 67002
##  4 Albé  67220         ""            ""                 67003
##  5 Alte… 67270         ""            ""                 67005
##  6 Alte… 67490         ""            ""                 67006
##  7 Alto… 67120         ""            ""                 67008
##  8 Altw… 67260         ""            ""                 67009
##  9 Andl… 67140         ""            ""                 67010
## 10 Arto… 67390         ""            ""                 67011
## # … with 504 more rows, and 6 more variables: arrondissement <chr>,
## #   canton <chr>, intercommunalité <chr>, superficie <dbl>,
## #   population <dbl>, densite <dbl>
{% endhighlight %}

Get the code [here](https://github.com/chodera/chodera.github.io/blob/master/assets/projects/2_scraping%20a_wikipedia_table/scraping_wikipedia.R){:target="_blank"}.
