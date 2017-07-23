stopstoSP <- function(GTFSstops,epsg ,useSIRI = FALSE, SIRI = NULL){
  require(sp)
  require(rgdal)
  StopsForSIRI <- function(SIRI, stops = GTFSstops){
    if(length(unique(SIRI$LineRef))>1){
      print("ERROR: SIRI file contains more then one LineRef, there should only be 1 unique LineRef")
    }else{
      stops[which(stops$stop_code %in% SIRI$StopPointRef, arr.ind = TRUE),]
    }
  }
  if(useSIRI == FALSE & is.null(SIRI)){
    GTFSstops <- GTFSstops[!is.na(GTFSstops$stop_lon),]
    proj = make_EPSG()
    crs2 <- CRS(proj$prj4[proj$code == epsg])
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,GTFSstops$stop_lat),data = GTFSstops,proj4string = crs1)
    s <- spTransform(s, crs2)
  } else{
    GTFSstops <- GTFSstops[!is.na(GTFSstops$stop_lon),]
    GTFSstops <- StopsForSIRI(SIRI = SIRI, stops = GTFSstops)
    proj = make_EPSG()
    crs2 <- CRS(proj$prj4[proj$code == epsg])
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,GTFSstops$stop_lat),data = GTFSstops,proj4string = crs1)
    s <- spTransform(s, crs2)
  }
}
