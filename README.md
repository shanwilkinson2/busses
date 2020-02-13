# GM busses
Exploration of bus data from Open Data Manchester digging the data https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0

**life expectancy by bus route app is here: https://shanwilkinson2.shinyapps.io/life_exp_by_busroute/**

## files
### 01_read_in_files.R
Read in files from Open Data Manchester's dropbox. Some data transformation (eg to ensure spatial files are read as spatial). 

### 02_read_in_and_transform_fares.R
Fare data is stored in 'triangle' pattern (from & to in rows & columns). Want to make a function to transform this to to-from pairs that can be linked to geocoding of stops to work out price per km. Done on one route for one operator. Not yet functionalised. Some are pdf's with hand written comments...

### 03_combine_stops_and_routes.R
It's a journey through several files to see which routes call at which stops. This file combines them & produces a combined dataset 'joined_stops_routes.geojson'.

### 04 add variables to stops.R
Add male & female life expectancy (from PHE fingertips) & deprivation (IMD 2019) to bus stops. Also includes MSOA ( Middle Super Output Area - for life expectancy merging) & LSOA (Lower Super Output Area - for IMD merging) & local authority. Produces 'stops_extradata.geojson'. 

### 05 map stops & routes.R
preps an interactive shiny dashboard of life expectancy by route.
app is here: https://shanwilkinson2.shinyapps.io/life_exp_by_busroute/

### map_stops.R
Produces an interactive leaflet map of bus stops. 

## Data 
### joined_stops_routes.geojson
Result of '03_combine_stops_and_routes.R'. One file to show which routes call at which stops.

### stops_extradata.geojson
Result of '04 add variables to stops.R' all bus stops with added male & female life expectancy (from PHE fingertips) & deprivation (IMD 2019). 

## To do
* **Shiny dashboard of life expectancy by bus stop along routes.** add a story showing greatest inequality, highest LE, lowest, drivers - PHE segment tool https://www.gov.uk/government/publications/segment-tool-2020-data-update/segment-tool-statistical-commentary-january-2020. 
* **Fare per km.** Which routes are most expensive? Which boroughs have the most expensive routes? Which routes have the greatest difference between operator? Needs fare data transformed first.  
* **Is fare influenced by deprivation along the route?** 

