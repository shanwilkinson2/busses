# combine routes to stops (via trips & stop times)

##########,
# data from open data manchester bus fare 
# https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0 

# definitions:
# https://developers.google.com/transit/gtfs/reference/
#################

# load packages 
  library(tidyr)
  library(sf)
  library(stringr)

# load files, takes a while...
  source("01_read_in_files.R")

###################### 

# stops(stop_id) -> 14115
#   stop_times(trip_id, stop_id) -> (has no unique id) 3,253,507
#     trips(trip_id, route_id) -> 77,722
#       routes(route_id) 794

# create a joined dataset
  joined <- trips %>%
    group_by(route_id) %>%
   # just get the first trip per route
    filter(trip_id == min(trip_id)) %>%
    ungroup() %>%
    left_join(routes, by = "route_id") %>%
    left_join(stop_times, by = "trip_id") %>% # getting 33,484 from this - because one row per stop per route
    left_join(stops, by = "stop_id") %>%
    st_as_sf() %>% 
    mutate(arrival_time = as.character(arrival_time), 
           departure_time = as.character(departure_time))
 
# # write joined dataset to ESRI shapefile. 
#   # abbreviates field names, drops arrival time & departure time
#   st_write(joined, "joined_stops_routes.shp")
  
# # code to read in joined dataset & put names back  
#   imported_joined <- st_read("joined_stops_routes.shp")
#   names(imported_joined) <- c("route_id", "service_id", "trip_id", "trip_headsign",
#                     "agency_id", "route_short_name", "route_long_name", "route_type",      
#                     "arrival_time", "departure_time", "stop_id", "stop_sequence",  
#                     "pickup_type", "drop_off_type", "stop_name", "geometry"
#                     )

  # write joined dataset as geojson 
  # keeps field names the same unlike esri shp but still drops time fields 
  # but ok converted to character
  st_write(joined, "joined_stops_routes.geojson")
  
  
##########################
  
# add service id to trip
  # stop_times <- left_join(stop_times, trips, by = "trip_id")
  stop_times <- stop_times %>%
    group_by(trip_id) %>%
    nest() %>%
    left_join(trips, by = "trip_id") %>%
    left_join(routes, by = "route_id")

  # trips are different times but same route so just keep first trip per route
  stop_times <- stop_times %>%
    group_by(route_id) %>%
    filter(trip_id == min(trip_id)) %>%
    
    stop_times1 <- stop_times %>%  
    unnest() %>%
    left_join(stops, by = "stop_id") %>%
    #st_as_sf() %>%
    group_by(trip_id) %>%
    nest(arrival_time, departure_time, stop_id, stop_sequence, pickup_type, drop_off_type)
  
# make a shorter dataset
  # these aren't necessarily all Bolton routes, just those with Bolton in the name
  bolton_routes <- routes %>%
    filter(str_detect(route_long_name, "Bolton"))

  stop_times_short <- stop_times %>%
    filter(route_id %in% bolton_routes$route_id)
  
# make a upershort dataset
  stop_times_supershort <- stop_times_short[c(25, 123, 500, 780, 1000, 2600, 5345, 6341),]

# add stop geocode
  stop_times_supershort <- unnest(stop_times_supershort) %>%
    left_join(stops, by = "stop_id") %>%
    st_as_sf()

# this works!!!  
  stop_times_supershort2 <- stop_times_supershort %>%
    group_by(trip_id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    left_join(st_drop_geometry(stop_times_supershort), by = "trip_id")

# route_id - route plus direction plus operator
# service_id - route plus date/ day on which it's offered

# make routes into a linestring  
  stop_times_short <- unnest(stop_times_supershort) %>%
    left_join(stops, by = "stop_id") %>%
    st_as_sf() %>%
    group_by(trip_id) %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING") %>%
    left_join(st_drop_geometry(stop_times_supershort), by = "trip_id")    

# map #######################
  
 library(leaflet)
  
  # route map
  stop_times_supershort2 %>%
    # filter(route_short_name == "500") %>%
    leaflet() %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addPolylines(weight = 2, color = "red", popup = ~(glue("{route_short_name}<br>{route_long_name}")))
  
