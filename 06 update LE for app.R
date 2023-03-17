# update life expectancy 

library(dplyr)
library(sf)
library(data.table)
library(tidyr)
# library(fingertipsR)

# ASSUMES ONLY LIFE EXPECTANCY IS CURRENTLY ON LCOAL HEALTH, CHECK THAT FIRST #######
# https://fingertips.phe.org.uk/profile/local-health/data#page/0/gid/1938133180/ati/3/iid/93744/age/28/sex/4/cid/4/tbm/1 

# load data being used by app
  app_data <- readRDS("./life_exp_by_busroute/shiny_life_exp_stop.rds")
  
  # generated in '04 add variables to stops'
    stops <- st_read("stops_extradata.geojson")

# get life expectancy by MSOA from local health profile 
 
  # indicator ids to filter by - check if healthy life expectancy is back on link above
  wanted_ids <- c(93283, # life expectancy
                  93298) # healthy life expectancy
  
  life_exp <- fread("https://fingertips.phe.org.uk/api/all_data/csv/by_profile_id?child_area_type_id=3&parent_area_type_id=15&profile_id=143") %>%
    janitor::clean_names(case = "upper_camel") # upper camel case used in API json output & fingertipsR
  
  life_exp2 <- life_exp %>%
    # keep MSOA level only (smallest available)
    filter(AreaType == "MSOA" & IndicatorId %in%wanted_ids) %>%
    # reduce number of variables
    select(AreaCode, IndicatorId, Sex, Value) %>%
    # put LE/ HLE in columns
    pivot_wider(id_cols = c(AreaCode, Sex), 
                names_from = IndicatorId, values_from = Value) %>%
    # change col names from number to short name
    rename(le = "93283"
           # , hle = "93298" # hle has gone off local health
    ) %>%
    # pivot so only 1 row per MSOA
    pivot_wider(id_cols = AreaCode, 
                names_from = c(Sex), 
                values_from = le) %>%
    janitor::clean_names() %>%
    rename(le_male = male, le_female = female)
    
  
  # # get life expectancy, HLE (85+ oldest age grp)
  # # AreaTypeID 3 = MSOA
  # life_exp <- 
  #   # get data from PHE fingertips
  #   fingertips_data(
  #     IndicatorID = c(93283), 
  #     ProfileID = 143, 
  #     AreaTypeID = 3)  %>%   
  #   # keep MSOA level only (smallest available)
  #   filter(AreaType == "MSOA") %>%
  #   # reduce number of variables
  #   select(AreaCode, IndicatorID, Sex, Value) %>%
  #   # put LE/ HLE in columns
  #   pivot_wider(id_cols = c(AreaCode, Sex), 
  #               names_from = IndicatorID, values_from = Value) %>%
  #   # change col names from number to short name
  #   rename(le = "93283"
  #          # , hle = "93298" # hle has gone off local health
  #   ) %>%
  #   # pivot so only 1 row per MSOA
  #   pivot_wider(id_cols = AreaCode, 
  #               names_from = c(Sex), 
  #               values_from = le) %>%
  #   janitor::clean_names() %>%
  #   rename(le_male = male, le_female = female)
  
  # remove previous LE, then join to stops  
  stops <- stops %>%
    select(-c(le_male, le_female, not_good_health_male, not_good_health_female)) %>%
    # merge in life expectancy
    left_join(life_exp2, by = c("msoa11cd" = "area_code")) %>%
    # calculate years not in good health 
    # ie difference between life expectancy & healthy life expectancy
    mutate(not_good_health_male = le_male-hle_male, not_good_health_female = le_female-hle_female)
    
  # proceed with "05 prep data for app" but don't reread 'stops' in, use the one created above