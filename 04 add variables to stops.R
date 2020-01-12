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
library(openxlsx)
library(janitor)
library(fingertipsR)
library(tidyr)

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
  select(c(1:3, 5:7, 12:14))

# remove boundary files now merged in
  rm(lsoa)
  rm(msoa)
  
# import imd
  # read.xlsx imports from web link
  imd <- read.xlsx("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833978/File_5_-_IoD2019_Scores.xlsx",
                   sheet = 2) %>%
    select(-2) %>% # drop lsoa name as already got it 
    clean_names() %>%
    select(c(1:4)) # drop everything except LA details & overall imd
  
# merge in imd
  stops <- left_join(stops, imd, by = c("LSOA11CD"="lsoa_code_2011"))

# remove imd as merged now.   
  rm(imd) 
  
#############
  
# import life expectancy
  # # list of available indicators
  # indicators() %>%
  #   filter(IndicatorID == 93283) %>%
  #   View()

  # # list of available areas
  #   indicator_areatypes(IndicatorID = 93283)   

  # get life expectancy
    # AreaTypeID 3 = MSOA
    life_exp <- fingertips_data(
        IndicatorID = 93283, 
        ProfileID = 143, 
        AreaTypeID = 3) %>%    
      filter(AreaType == "MSOA") %>%
      select(AreaCode, Sex, Value) %>%
      spread(key = Sex, value = Value) %>%
      select(AreaCode, female_life_exp = Female, male_life_exp = Male)
 
  # merge in life expectancy
    stops <- left_join(stops, life_exp, by = c("msoa11cd" = "AreaCode"))
    
##########
    
    # export joined file
    st_write(stops, "stops_extradata.geojson")
    
  