# map stops

# load packages for reading in file
  library(readr)
  library(dplyr)
  library(sf)
# load packages for map
  library(leaflet)
  library(leaflet.extras)
  library(glue)

  
# load bus stop file
  stops <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AADMaJKBTuHlp5jdXBmgRT8ya/gtdf-out/stops.txt?dl=1") %>%
    st_as_sf(coords = c("stop_lon", "stop_lat")) %>%
    st_set_crs(4326) %>%
    select(-stop_code, -stop_url) # drop code & url just seem to be for text for times
  
# map of stops
  mypopup <- (glue("<b>Stop id:</b> {stops$stop_id}<br>
                      <b>Stop name:</b> {stops$stop_name}"))
  leaflet(stops) %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(radius = 3, color = "magenta", 
                     clusterOptions = markerClusterOptions(), 
                     popup = ~mypopup)