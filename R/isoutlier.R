is.outlier <- function(spSIRI, trip){
  siridfch <- gConvexHull(spSIRI[spSIRI@data$trip_id == trip,])
  if(!is.null(siridfch)){
    cent1 <- gCentroid(siridfch)
    centbuffer <- gBuffer(cent1, width = 75)
    outlier <- gWithin(spSIRI, centbuffer)
    return(outlier)
  }else{
    print(paste('failed trip: ', trip))
  }
}
