

# start_end_dates #########################
start_end_dates <- function(start_date = "20170101", end_date = Sys.Date(), by_time = "month") {
# DESCRIPTION:  creates a sequence of multiple start and end dates, mostly for looping through to repeat processes that must be done on subsets of a timeseries dataset
# INPUT: start_date and end_date as character strings like this: "20170101"; end_date defaults to Sys.Date(). use by_time to specify time interval to be spanned on each n row. 
# OUTPUT: a 2 x n data fram with columns start_date and end_date
first_date <- as.Date(start_date, format='%Y%m%d')
if(as.Date(end_date, format='%Y%m%d') == Sys.Date()) {
end_date <- Sys.Date() %m-% months(1)
} else {
  end_date <- as.Date(end_date, format='%Y%m%d')
}

start_dates <- as.Date(seq(first_date, end_date, by = by_time))
end_dates <- (ceiling_date(start_dates, by_time)) - days(1)

start_end_dates <- data.frame(start_date = start_dates, end_date = end_dates) %>% 
  mutate(end_date = ifelse(end_date < Sys.Date(), end_date, Sys.Date()),
         end_date = as.Date(end_date, "1970-01-01"),
         start_date = gsub("-", "", start_date),
         end_date = gsub("-", "", end_date))
}



# add_dawn_dusk ###########################
#' add_dawn_dusk
#' 
#' add dawn and dusk times to a data frame with coordinates and time
#'
#' @param df data frame that has at least columns timestamp, latitude, longitude. NOTE makes df into sf object --- don't input a sf object
#'
#' @return sf object with same columns as input plus 2 columns (dawn.time, dusk.time). output CRS is "+proj=longlat +datum=WGS84"
#' @export
#'
#' @examples
add_dawn_dusk<-function(df){
  # REQUIRES: maptools
  # REQUIRED FOR: assigning inlight to each GPS point (Assign_inlight()); calcaulating the number of hours of eelgrass availability per day for Tomales Bay (add_eelgrass_hours_day())
  
  
  projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  df.sp <- st_as_sf(x = df,                         
                    coords = c("longitude", "latitude"),
                    crs = projcrs)
  
  
  
  dawn = data.frame(crepuscule(df.sp, df.sp$timestamp, solarDep = 6, direction = "dawn", POSIXct.out = TRUE))
  dusk = data.frame(crepuscule(df.sp, df.sp$timestamp, solarDep = 6, direction = "dusk", POSIXct.out = TRUE))
  
  dawn.dusk <- cbind(dplyr::select(dawn, dawn.time = time), dplyr::select(dusk, dusk.time = time))
  hetp_dawn.dusk <- cbind(df, dawn.dusk) 
}

#hetp_dd <- add_dawn_dusk(hetp_use)

#foo <- add_dawn_dusk(study_days_blake)


# assign_inlight ###################################
assign_inlight <- function(df) {
# DESCRIPTION: assign daylight T or F to each point, based on fields created by add_dawn_dusk()
# PURPOSE: 
# INPUT: df = data frame with at least columns timestamp, dawn.time, dusk.time
# OUTPUT: same as inpute plus 1 column (inlight T/F)

df <- df %>% 
  mutate(inlight = timestamp	%within%	interval(dawn.time, dusk.time))
  return(df)
}

#hetp_dd_inlight <- assign.inlight(hetp_dd)


