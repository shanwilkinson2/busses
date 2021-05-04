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
  # list of available indicators
  indicators() %>%
    filter(IndicatorID %in% c(93283, 93285, 93298)) %>%
    View()

  # # list of available areas
  #   indicator_areatypes(IndicatorID = 93283)   
  #   LE upper age band 85+ 	93285
  #   HLE upper age band 85+ only available, no 90+ available. 
  #   upper age groups has more variance so some effects on estimates

  # # get life expectancy - only
  #   # AreaTypeID 3 = MSOA
  #   life_exp <- fingertips_data(
  #       IndicatorID = 93283, 
  #       ProfileID = 143, 
  #       AreaTypeID = 3) %>%    
  #     filter(AreaType == "MSOA") %>%
  #     select(AreaCode, Sex, Value) %>%
  #     spread(key = Sex, value = Value) %>%
  #     select(AreaCode, female_life_exp = Female, male_life_exp = Male)

  # get life expectancy, HLE (85+ oldest age grp)
    # AreaTypeID 3 = MSOA
    life_exp <- 
      # get data from PHE fingertips
        fingertips_data(
          IndicatorID = c(93285, 93298), 
          ProfileID = 143, 
          AreaTypeID = 3)  %>%   
      # keep MSOA level only (smallest available)
        filter(AreaType == "MSOA") %>%
      # reduce number of variables
        select(AreaCode, IndicatorID, Sex, Value) %>%
      # put LE/ HLE in columns
        pivot_wider(id_cols = c(AreaCode, Sex), 
                    names_from = IndicatorID, values_from = Value) %>%
      # change col names from number to short name
        rename(le = "93285", hle = "93298") %>%
      # calculate years not in good health 
      # ie difference between life expectancy & healthy life expectancy
        mutate(not_good_health = le-hle) %>%
      # pivot so only 1 row per MSOA
        pivot_wider(id_cols = AreaCode, 
                    names_from = c(Sex), 
                    values_from = c(le:not_good_health)) %>%
      clean_names()
    
  # merge in life expectancy
    stops <- left_join(stops, life_exp, by = c("msoa11cd" = "area_code"))
    
##########
    
    # export joined file
    st_write(stops, "stops_extradata.geojson", delete_dsn = TRUE)
    
  