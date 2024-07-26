# Dependencies
library(leaflet)
library(plotly)
library(leafpop)
library(htmlwidgets)
library(tidyverse)
library(rnaturalearth)
library(here)

# Embedding URLs
port_maps <- read_csv(here("data", "ports_geocoded_map.csv"))

url_func<-function(PORT, URL){
  html <- paste0("<b><a href =", URL, ">", PORT, "</a></b>")
  return(html)
}

port_maps <- port_maps %>%
  mutate(port_url = map2(PORT, URL, url_func)) %>% 
  mutate(port_url = as.character(port_url))

# Portland test
port_maps %>%
  filter(PORT == "Portland, ME") %>%
  leaflet() %>%
  addProviderTiles(provider="Esri.OceanBasemap") %>%
  setView(lng= -70, lat=40, zoom = 4.5) %>%
  addMarkers(~lon, ~lat, popup = ~port_url)  #this works!

# Once PDFs are completed, upload as documents and use links as pop-ups
states <- ne_states(county = "united states of america", returnclass = "sf")

map <- leaflet(data = port_maps) %>%
  addProviderTiles(provider="Esri.OceanBasemap") %>%
  setView(lng= -70, lat = 41, zoom = 4.5) %>%
  addPolylines(data = states, color = "#535353", weight = 1, opacity = 0.25) %>%
  # setView(lng= -70.25682, lat=43.65910, zoom= 7) %>%
  addMarkers(~lon, ~lat, popup = ~port_url)

saveWidget(map, "Port_Map.html") # This will need to be uploaded as a document to the back end of the website

