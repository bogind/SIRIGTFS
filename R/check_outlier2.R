#' @export check_outlier2
check_outlier2 <- function(fullans){
  for(trip in unique(fullans$trip_id)){
    testdf <- fullans[fullans$trip_id == trip,]
    maxstop <- max(fullans$stop_sequence)
    obs <- nrow(testdf)
    if(obs/maxstop < 0.25){
      fullans$outlier[fullans$trip_id == trip] = 2
    }
    else{
      fullans$outlier[fullans$trip_id == trip] = 0
    }
  }
  fullans
}
