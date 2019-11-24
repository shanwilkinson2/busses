library(readr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(glue)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(openxlsx)
#library(otuSummary)

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
 
# fares. Not all funcitons will read from a web link. Seems to have a hidden sheet...
  diamond_fare1 <- read.xlsx("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AAAoGQzqPcROfzqsmHeVIFCxa/Fare%20tables/Diamond%20Bus/715_20190206205722.xlsx?dl=1", 
                             sheet = 2, startRow = 1)
  # doesn't work. lower triangle matrix
  # diamond_fare2 <- matrixConvert(diamond_fare1)

# transform fare table  
  varname <- diamond_fare1
  numrows <- nrow(varname-1)
  numcols <- ncol(varname)
  stop_names <- vector(mode='character',length=numcols)

  names(varnames) <- stop_names

  stops_melted <- data.frame(matrix(ncol = 3, nrow = numrows*numcols/2)) %>%
                           setNames(c("from", "to", "price"))

  # get stop names in a vector    
    for(i in 1:numcols) {
        stop_names[i] <- varname[i,i]
    }
  
  # take away the first row with just the first variable name in as got it now
  varname <- varname[-1,]  
  names(varname) <- stop_names
  row.names(varname) <- stop_names[2:length(stop_names)]
  varname <- data.matrix(varname)
  
  stops_melted <- cbind(which(!is.na(varname),arr.ind = TRUE),na.omit(as.vector(varname)))
  rownames(stops_melted) <- c()
  colnames(stops_melted)[3] <- "price"
  stops_melted <- as.data.frame(stops_melted)
  stops_melted[,"from"] <- NA
  stops_melted[,"to"] <- NA
  
  
  # get row / col indices into to / from names  
   for(i in 1:nrow(stops_melted)){
        stops_melted[i,4] <- row.names(varname)[stops_melted[i,1]]
        stops_melted[i,5] <- colnames(varname)[stops_melted[i,2]]
        }
  
  stops_melted <- stops_melted %>%
    select(from, to, price)
  
########## 
# which routes run to which stops?  
  

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



##

  
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
  