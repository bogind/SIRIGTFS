#' @export is.outlier
is.outlier <- function(spSIRI, trip, outliers){
  require("rgeos", quietly = TRUE)
  siridfch <- rgeos::gConvexHull(spSIRI[spSIRI@data$trip_id == trip,])
  if(!is.null(siridfch)){
    cent1 <- rgeos::gCentroid(siridfch)
    centbuffer <- rgeos::gBuffer(cent1, width = 75)
    outlier <- rgeos::gWithin(spSIRI, centbuffer)
    if(outlier){
      print(paste("trip_id ",trpi, " from lineref number" ,w, " is an outlier"))
      print("moving on.")
      outliers <<- c(outliers,paste(trip, " ,", i))
    }
  }else{
    print(paste('failed trip: ', trip))
  }
}
