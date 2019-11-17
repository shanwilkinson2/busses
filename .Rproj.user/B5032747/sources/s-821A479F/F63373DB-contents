library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

###########,
# open data manchester bus fare https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0 
#################

# read in files

# stops
  stops <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AADMaJKBTuHlp5jdXBmgRT8ya/gtdf-out/stops.txt?dl=1") %>%
    st_as_sf(coords = c("stop_lon", "stop_lat")) %>%
    st_set_crs(4326) %>%
    select(-stop_code, -stop_url) # drop code & url just seem to be for text for times
  
    # plot(st_geometry(stops))

# routes
  routes <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABZlyrwbd1Wu_gHV0QowUQfa/gtdf-out/routes.txt?dl=1")
    
# trips - big file
  trips <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABPdBmSL-T5Q6u72Ul5HrRxa/gtdf-out/trips.txt?dl=1")  

# stop times - big file 8MB
  stop_times <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AADqaMDXl8jsJJSUongzQiwqa/gtdf-out/stop_times.txt?dl=1")
 
########## 
# which routes run to which stops?  
  
#   stops_short <- stops %>%
#     st_drop_geometry() %>%
#     select(stop_id, stop_name) %>%
#     left_join()
#   
#  # stop_trip <- stop_times %>%
#  #    group_by(stop_id, trip_id) %>%
#  #    summarise(n()) %>%
#  #    select(stop_id, trip_id)
#   
#   # unique combinations of stop & trip 
#   stop_trip <- stop_times %>%
#     selet(-arrival_time, -departure_time) %>%
#     unique(stop_times[,c("stop_id", "trip_id")]) 
#  
#   
#   trips %>%
#     group_by(service_id) %>%
#     summarise(num_trips = n())
#     
#   nrow(unique(trips[,c("service_id", "trip_id")]))
#    
# trip_stop <- left_join(trips, stop_route)  
#   
#     
# stop_route <- stops %>%
#   st_drop_geometry() %>%
#   select(stop_id) %>%
#   left_join(stop_trip, by = )
 
### 

# # add stop location to stop times - takes too long
#   stop_times <- left_join(stop_times, stops, by = "stop_id") %>%
#   st_as_sf()
  
# add service id to trip
  # stop_times <- left_join(stop_times, trips, by = "trip_id")
  stop_times <- stop_times %>%
    group_by(trip_id) %>%
    nest()
  stop_times <- left_join(stop_times, trips, by = "trip_id")
  stop_times <- left_join(stop_times, routes, by = "route_id" )
  
#   stop_times <- stop_times %>%
#     nest(-route_id, -service_id, -trip_headsign)
#   # service_id seems to be a code version of route_id, ie a combination of route number & operator  
# stop_times <- unnest(stop_times)

  
# make a shorter dataset
  bolton_routes <- routes %>%
    filter(str_detect(route_long_name, "Bolton"))
  
stop_times_short <- stop_times %>%
  filter(route_id %in% bolton_routes$route_id)

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

    
# # doesn't work 
#      stop_times_supershort1 <- stop_times_supershort %>%
#       filter(trip_id == min(trip_id)) %>%
#       map(nest(geometry))
#       st_linestring(geometry)
#       st_multilinestring()

#   stop_times_supershort <- stop_times_supershort %>%
#     nest(-(trip_id:route_type))
# 
#  
# stop_times_supershort <- st_multilinestring(stop_times_supershort)
# 
# stop_times_supershort1 <- stop_times_supershort %>%
#   filter(trip_id == min(trip_id)) %>%
#   select(trip_id, stop_id, stop_name, geometry) %>%
#   st_cast(geometry, to = "LINESTRING")


##

ps = rbind(c(10, 10), c(10, 5), c(11,9), c(11,10))
  
# map #######################

# map of stops
  mypopup <- (glue("<b>Stop id:</b> {stops$stop_id}<br>
                    <b>Stop name:</b> {stops$stop_name}"))
  leaflet(stops) %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addCircleMarkers(radius = 3, color = "magenta", 
                     clusterOptions = markerClusterOptions(), 
                     popup = ~mypopup)
    
# route map
  stop_times_supershort2 %>%
    # filter(route_short_name == "500") %>%
  leaflet() %>%
    addProviderTiles("Stamen.TonerLite") %>%
    addPolylines(weight = 2, color = "red", popup = ~(glue("{route_short_name}<br>{route_long_name}")))
  