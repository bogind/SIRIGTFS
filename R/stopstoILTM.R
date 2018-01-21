#' @importFrom sp CRS SpatialPointsDataFrame spTransform
#' @importFrom rgdal make_EPSG
#' @export

stopstoILTM <- function(GTFSstops. = GTFSstops, useSIRI = FALSE, SIRI = NULL){
  require(sp, quietly = TRUE)
  require(rgdal, quietly = TRUE)
  if(useSIRI == FALSE & is.null(SIRI)){
    GTFSstops. <- GTFSstops.[!is.na(GTFSstops.$stop_lon),]
    proj = rgdal::make_EPSG()
    israelTM <- sp::CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(GTFSstops.$stop_lon,GTFSstops.$stop_lat),data = GTFSstops.,proj4string = crs1)
    s <- sp::spTransform(s, israelTM)
    return(s)
  } else{
    GTFSstops. <- GTFSstops.[!is.na(GTFSstops.$stop_lon),]
    GTFSstops. <- SIRItoGTFS::StopsForSIRI(SIRI = SIRI, stops = GTFSstops.)
    proj = rgdal::make_EPSG()
    israelTM <- sp::CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(GTFSstops.$stop_lon,GTFSstops.$stop_lat),data = GTFSstops.,proj4string = crs1)
    s <- sp::spTransform(s, israelTM)
    return(s)
  }
}
