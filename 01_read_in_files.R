# read in files

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

  # plot(st_geometry(stops))

# routes 
  # combination of roue (both in & out) & operator
  routes <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABZlyrwbd1Wu_gHV0QowUQfa/gtdf-out/routes.txt?dl=1")

# trips - big file
  # combination of bus route plus date range it runs on
  trips <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABPdBmSL-T5Q6u72Ul5HrRxa/gtdf-out/trips.txt?dl=1")  

# stop times - big file 8MB
  # link stops to route
  stop_times <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AADqaMDXl8jsJJSUongzQiwqa/gtdf-out/stop_times.txt?dl=1")

# bus companies
  agencies <- read_csv("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABaLjUO7xg7sYJ1oSiHJEpTa/gtdf-out/agency.txt?dl=1")  
