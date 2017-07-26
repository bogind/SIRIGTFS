is.outlier <- function(spSIRI, trip){
  requireNamespace("rgeos", quietly = TRUE)
  siridfch <- rgeos::gConvexHull(spSIRI[spSIRI@data$trip_id == trip,])
  if(!is.null(siridfch)){
    cent1 <- rgeos::gCentroid(siridfch)
    centbuffer <- rgeos::gBuffer(cent1, width = 75)
    outlier <- rgeos::gWithin(spSIRI, centbuffer)
    return(outlier)
  }else{
    print(paste('failed trip: ', trip))
  }
}
