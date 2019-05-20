---
layout: post
title: "History of buildings in Strasbourg"
output:
  html_document:
    df_print: paged
image: assets/images/stras_map_opt.png
normal: true
show: true
logo: r
---

Recently, I came across the crowd-sourcing project [Archi-Wiki](https://www.archi-wiki.org/){:target="_blank"} which documents the history of buildings and places of a city, together with background information on the architect, successive transformations and a collection of photos. Being based in Strasbourg, this site especially contains rich information on places in Strasbourg and I decided to use the data to visualize the history of buildings in the city (inspired by projects such as [Histoire du bâti Parisien](https://www.comeetie.fr/galerie/BatiParis/#12/48.8589/2.3491){:target="_blank"} and [Block by Block, Brooklyn's Past an Present](https://www.bklynr.com/block-by-block-brooklyns-past-and-present/){:target="_blank"}).

As the API turned out not to be functioning after contacting the host, the only way to get the data was to scrape all the websites containing information on the individual buildings (more than 13,000). Out of the information available I was particularly interested in the date of construction in the infobox. There is as well a little leaflet map on each page, centered on the specific building which is why I also scraped the metadata of that map (latitude and longitude of marker). However, this data turned out to be quite unprecise as some markers are not within the polygons of the building footprints.

As a remedy, the addresses were geolocalised with the help of the official address register of the city of Strasbourg, which is available on their [open data portal](https://github.com/chodera/chodera.github.io/tree/master/assets/projects/9_archi-wiki){:target="_blank"}. Therefore, there was luckily no need for using geocoding service. Finally, spatial data on the building footprints comes from OpenStreetMap and was obtained via [BBBike](https://download.bbbike.org/osm/bbbike/){:target="_blank"}.

This project is still being developed and the map underneath is just a first impression of how the map will look like in the end. By joining the various data sources, there is a considerate amount of data being lost (from 10,190 down to 3,024 addresses); there is in particular need for better harmonizing the data on addresses from Archi-Wiki and the official register.

<iframe style="width:100%;" height="600" src="{{site.baseurl}}/assets/leaflet/map_stras" frameborder="0" allowfullscreen></iframe>

Get the preliminary data sets and scripts [here](https://github.com/chodera/chodera.github.io/tree/master/assets/projects/9_archi-wiki){:target="_blank"}.
