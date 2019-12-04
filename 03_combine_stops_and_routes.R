# combine routes to stops (via trips & stop times)

# load packages 
  library(tidyr)
  library(sf)
  library(stringr)

# load files, takes a while...
  source("01_read_in_files.R")

### 

# # add stop location to stop times - takes too long
#   stop_times <- left_join(stop_times, stops, by = "stop_id") %>%
#   st_as_sf()

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
  
