stopstoILTM <- function(GTFSstops, useSIRI = FALSE, SIRI = NULL){
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
    israelTM <- CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,GTFSstops$stop_lat),data = GTFSstops,proj4string = crs1)
    s <- spTransform(s, israelTM)
  } else{
    GTFSstops <- GTFSstops[!is.na(GTFSstops$stop_lon),]
    GTFSstops <- StopsForSIRI(SIRI = SIRI, stops = GTFSstops)
    proj = make_EPSG()
    israelTM <- CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,GTFSstops$stop_lat),data = GTFSstops,proj4string = crs1)
    s <- spTransform(s, israelTM)
  }
}
