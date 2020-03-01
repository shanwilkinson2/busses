# prep data for app
# stops & routes with life expectancy data

# load packages
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)

# read in files
  stops <- st_read("stops_extradata.geojson")
  stops_routes <- st_read("joined_stops_routes.geojson") 

# In/ out/ different operators have different numbers of stops
  stops_routes %>%
    st_drop_geometry() %>%
    filter(route_short_name ==575) %>%
    group_by(route_id) %>%
    summarise(n())
  
# duplicate route numbers? - Yes
# so need to filter by route_short_name & route_long_name not just route_short_name  
  stops_routes %>%
    st_drop_geometry() %>%
    select(route_id: route_long_name, -trip_id) %>%
    # keep outbound only (remove inbound)
    filter(str_detect(route_id, ":O:$")) %>%
    # keep one row per route id only
    group_by(route_id) %>%
      slice(1) %>%
    ungroup() %>%
    # keep one row per route with same name & number 
      # (deletes multiple operators over same route)
    group_by(route_short_name, route_long_name) %>%
      slice(1) %>%
    ungroup() %>%
    group_by(route_short_name) %>%
      mutate(n = n()) %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    View()
   

# join stops with stops with life expectancy added
  #### GETTING DUPLICATE OF STOP INFO
  stops_routes_joined <- 
    left_join(stops_routes, st_drop_geometry(stops), by = "stop_id") %>%
    # wnat to get rid of the multiple operator/ in /outbound versions & just keep longest
    # BUT duplicate route numbers
    # keep outbound only (remove inbound)
    filter(str_detect(route_id, ":O:$")) %>%
    group_by(route_id) %>%
    mutate(stop_sequence = as.numeric(stop_sequence), 
           route_short_long_name = paste(route_short_name, ": ", route_long_name),
           num_stops = n()) %>%
    ungroup() %>%
    group_by(route_short_name, route_long_name) %>%
    filter(num_stops == max(num_stops))
  
# make one life expectancy column instead of two so can filter out unwanted one
  # was struggling with switching between columns for colouring & label
  stops_routes_joined2 <- stops_routes_joined %>%
    gather(key = "life_exp_gender", value = "life_exp_val", 
           c(male_life_exp:female_life_exp)) %>%
    mutate(life_exp_gender = recode(life_exp_gender, "male_life_exp" = "male", 
                                    "female_life_exp" = "female")) %>%
    select(route_id, agency_id:route_long_name, stop_id, stop_sequence, stop_name:life_exp_val)

##########
  # pivot so one gender col, & one stat col 
    # was struggling with switching between columns for colouring & label
  stops_routes_joined3 <- stops_routes_joined %>%
    # pivot_longer(cols = le_male:le_female, 
    #              names_to = "life_exp_stat", values_to = "life_exp_val")
    gather(key = "life_exp_gender", value = "life_exp_val",
           c(le_male:not_good_health_female))

# save data for shiny app    
  st_write(stops_routes_joined2, "life_exp_by_busroute\\shiny_life_exp_stop.geojson")
  
