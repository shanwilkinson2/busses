# map stops & routes

# load packages
  library(sf)
  library(dplyr)
  library(leaflet)
  library(RColorBrewer)
  library(glue)

# read in files
  stops <- st_read("stops_extradata.geojson")
  stops_routes <- st_read("joined_stops_routes.geojson") 

 # was going to filter out different operatorts/ in/ out route
    # but different numbers of stops.
    # seemed to work fine without bothering.
  stops_routes %>%
    st_drop_geometry() %>%
    filter(route_short_name ==575) %>%
    group_by(route_id) %>%
    summarise(n())
 stops_routes %>%
   st_drop_geometry() %>%
   filter(route_short_name ==575) %>%
   select(route_id, stop_name)
  stops_routes %>%
    st_drop_geometry() %>%
    group_by(route_short_name) %>%
    filter(min(route_id))

# join
  stops_life_exp <- stops %>%
    st_drop_geometry() %>%
    select(stop_id, male_life_exp, female_life_exp)
  
  # test one route only
  stops_routes_short <- stops_routes %>%
    filter(route_id == "NOR: 575:O:") # 42 stops
  stops_routes_short <- stops_routes %>%
    filter(route_id == "GTB: 575:O:") # 27 stops
  
  # join
  stops_routes_short <- 
    left_join(stops_routes_short, stops_life_exp, by = "stop_id")

# map male life expectancy along route selected above
  
  pal <- colorNumeric(palette = "BuPu", domain = stops_routes_short$male_life_exp)
  mylabel <- glue("{stops_routes_short$stop_name}
                  Male life expectancy: {stops_routes_short$male_life_exp}")
  
  leaflet(stops_routes_short) %>%  
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(radius = 5, fillColor = ~pal(male_life_exp), 
    label = ~mylabel, weight = 2, fillOpacity = 0.8, color = "black")
