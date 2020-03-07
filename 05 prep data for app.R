# prep data for app
# stops & routes with life expectancy data

# load packages
  library(sf)
  library(dplyr)
  library(tidyr)
  library(stringr)

# read in files
  stops <- st_read("stops_extradata.geojson") %>%
    st_drop_geometry() %>%
    select(stop_id, le_male:not_good_health_female)
  
  stops_routes <- st_read("joined_stops_routes.geojson") %>%
    select(-c(trip_id, route_type:departure_time, pickup_type:drop_off_type))

# check if in/ out/ different operators have different numbers of stops
  stops_routes %>%
    st_drop_geometry() %>%
    filter(route_short_name ==575) %>%
    group_by(route_id) %>%
    summarise(n())
  
# check if duplicate route numbers? - Yes
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
  stops_routes_joined <- 
    left_join(stops_routes, stops, by = "stop_id") %>%
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
  
  # remove pre-merge files, as now merged
    rm(stops)
    rm(stops_routes)

  # pivot so one gender col, one stat col, one value col 
    # was struggling with switching between columns for colouring & label
  stops_routes_joined2 <- stops_routes_joined %>%
    # can't get pivot_longer to work but gather works ok.
    gather(key = "life_exp_stat", value = "life_exp_val",
           le_male:not_good_health_female) %>%
    mutate(life_exp_stat =  str_replace(life_exp_stat, 
                                        "not_good_health", "notgoodhealth")) %>%
    separate(col = life_exp_stat, 
             into = c("life_exp_stat", "life_exp_gender"), 
             sep = "_") %>%
    mutate(life_exp_stat =  str_replace(life_exp_stat, 
                                        "notgoodhealth", "years not in good health"),
           life_exp_stat =  str_replace(life_exp_stat, 
                                        "hle", "healthy life expectancy"),
           life_exp_stat =  str_replace(life_exp_stat, 
                                        "le", "life expectancy"))
   
# save data for shiny app    
  # rds file super much smaller than geojson also R doesn't need to translate it
  saveRDS(stops_routes_joined2, "life_exp_by_busroute\\shiny_life_exp_stop.rds")
