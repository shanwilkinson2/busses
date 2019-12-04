# read in & transform fare data

###########,
# data from open data manchester bus fare 
# https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AABOIfA4vZyTzkZqbsTUPaFGa?dl=0 

# definitions:
# https://developers.google.com/transit/gtfs/reference/
#################

# load packages
  library(openxlsx)

# read in fares 

  # different operators have fares in slightly different formats & file types
  # but all are in triangle format, ie from in col to in row
  # note: not all excel reader funcitons will read from a web link. 

# Diamond bus ##################################################
  # route num is first part of filename (before underscore)

  # hidden sheet on sheet 1
  diamond_fare_715 <- read.xlsx("https://www.dropbox.com/sh/4djlyzcdo0ytpcf/AAAoGQzqPcROfzqsmHeVIFCxa/Fare%20tables/Diamond%20Bus/715_20190206205722.xlsx?dl=1", 
                             sheet = 2, startRow = 1)

# transform fare table  
# would like to make this into a function  
  # set up parameters
  varname <- diamond_fare_715
  numrows <- nrow(varname)-1 # can't go to & from itself so first stop missed from row
  numcols <- ncol(varname)
  stop_names <- vector(mode='character',length=numcols)
  names(varname) <- stop_names

  # stops_melted <- data.frame(matrix(ncol = 3, nrow = numrows*numcols/2)) %>%
  #   setNames(c("from", "to", "price"))

# get stop names in a vector    
  for(i in 1:numcols) {
    stop_names[i] <- varname[i,i]
  }

# use stop names to set row/ col names    
  # take away the first row with just the first variable name in as got it now
    varname <- varname[-1,]  
  # set col names
    names(varname) <- stop_names 
  # set row names (to use them later)
    row.names(varname) <- stop_names[2:length(stop_names)]
  # make into a matrix of just prices (stops are in row/ col names)
    varname <- data.matrix(varname)
    
# reshape fare table from triangle to from-to (to calculate distances later)
  stops_melted <- cbind(which(!is.na(varname),arr.ind = TRUE),na.omit(as.vector(varname)))
  rownames(stops_melted) <- c() # get rid of row names
  # colnames(stops_melted)[3] <- "price"
   stops_melted <- as.data.frame(stops_melted)

  # get row / col indices into to / from names  
    for(i in 1:nrow(stops_melted)){
      stops_melted[i,4] <- row.names(varname)[stops_melted[i,1]]
      stops_melted[i,5] <- colnames(varname)[stops_melted[i,2]]
    }

  # rename cols
    names(stops_melted) <- c("from_row", "to_col", "price", "from_name", "to_name")
  # add route num & agency  
    stops_melted$route_short_name <- "715"
    stops_melted$agency_id <- "GTB"

# next... 
    # combine with geocoded stop location 
    # calculate distances so price per km
  