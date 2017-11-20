#' @export check_outlier
check_outlier <- function(spSIRI){
  require("rgeos", quietly = TRUE)
  testsp <- spSIRI
  for(trip in unique(testsp@data$trip_id)){
    tested <- testsp[testsp@data$trip_id == trip,]
  siridfch <- rgeos::gConvexHull(tested)

  if(!is.null(siridfch)){
    cent1 <- rgeos::gCentroid(siridfch)
    centbuffer <- rgeos::gBuffer(cent1, width = 75)
    inlier <- rgeos::gWithin(tested, centbuffer)
    short <- (nrow(tested@data) <= 10)
    first <- min(tested@data$RecordedAtTime)
    last <- max(tested@data$RecordedAtTime)
    delta <- as.numeric(difftime(last,first,units = "mins"))
    if(inlier & short & delta <= 30){
      testsp@data$outlier[testsp@data$trip_id == trip] = 1
    }else{
      testsp@data$outlier[testsp@data$trip_id == trip] = 0
      }
    }
  }
  testsp
}
