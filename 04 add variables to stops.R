###########,
# data from open data manchester bus fare 
# https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0 

# definitions:
# https://developers.google.com/transit/gtfs/reference/
#################

# load packages
library(readr)
library(dplyr)
library(sf)

# bus stops
  stops <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AADMaJKBTuHlp5jdXBmgRT8ya/gtdf-out/stops.txt?dl=1") %>%
    st_as_sf(coords = c("stop_lon", "stop_lat")) %>%
    st_set_crs(4326) %>%
    select(-stop_code, -stop_url) # drop code & url just seem to be for text for times

# lsoas clipped to 20m
  # IMD at lsoa
  lsoa <- st_read("https://opendata.arcgis.com/datasets/e993add3f1944437bc91ec7c76100c63_0.geojson")

# msoa clipped to 20m
  # life expectancy at msoa
  msoa <- st_read("https://opendata.arcgis.com/datasets/29fdaa2efced40378ce8173b411aeb0e_2.geojson")

stops <- stops %>%
  st_join(lsoa, join = st_within) %>%
  st_join(msoa, join = st_within) %>%
  select(c(1:3, 5:7, 13:14))

# export joined file
  st_write(stops, "stops_extradata.geojson")

# remove boundary files now merged in
  rm(lsoa)
  rm(msoa)