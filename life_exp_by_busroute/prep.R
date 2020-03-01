library(sf)
# dataset
stops_routes_joined2 <- st_read("shiny_life_exp_stop.geojson")

saveRDS(stops_routes_joined2, "life_exp_by_busroute/data/stops_routes_joined2.rds")
