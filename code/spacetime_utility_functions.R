

#' add_dawn_dusk_inlight
#' 
#' add dawn and dusk times and whether or not a timestamp happened during daylight to a data frame with coordinates and time
#'
#' @param df data frame that has at least columns timestamp, latitude, longitude. NOTE makes df into sf object --- don't input a sf object
#'
#' 
#' @return sf object with same columns as input plus 3 columns (dawn.time, dusk.time, inlight). output CRS is "+proj=longlat +datum=WGS84"
#'
#' @examples
#' # dawn and dusk time for today at Cypress Grove
#' add_dawn_dusk(data.frame(timestamp = Sys.time(), latitude =  38.165709, longitude = -122.900829))
#' # should result in error message about missing field
#' add_dawn_dusk(data.frame(timestamp = Sys.time(), latitude =  38.165709))
add_dawn_dusk_inlight <- function(df){
  
  if(!all(c("timestamp", "latitude", "longitude") %in% names(df))) {
    stop("The input data frame does not have the three required fields: timestamp, latitude, longitude")
  }

  projcrs <- "+proj=longlat +datum=WGS84"
  df.sp <- sf::st_as_sf(x = df,                         
                    coords = c("longitude", "latitude"),
                    crs = projcrs)
  
  dawn = data.frame(suntools::crepuscule(df.sp, df.sp$timestamp, solarDep = 6, direction = "dawn", POSIXct.out = TRUE))
  dusk = data.frame(suntools::crepuscule(df.sp, df.sp$timestamp, solarDep = 6, direction = "dusk", POSIXct.out = TRUE))
  
  dawn_dusk <- cbind(dplyr::select(dawn, dawn.time = time), dplyr::select(dusk, dusk.time = time))
  dawn_dusk_out <- cbind(df, dawn_dusk) %>% 
    mutate(inlight = timestamp	%within%	interval(dawn.time, dusk.time))
return(dawn_dusk_out)
  }



