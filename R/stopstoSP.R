stopstoSP <- function(GTFSstops,epsg ,useSIRI = FALSE, SIRI = NULL){
  require(sp, quietly = TRUE)
  require(rgdal, quietly = TRUE)
  if(useSIRI == FALSE & is.null(SIRI)){
    GTFSstops <- GTFSstops[!is.na(GTFSstops$stop_lon),]
    proj = rgdal::make_EPSG()
    crs2 <- sp::CRS(proj$prj4[proj$code == epsg])
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,
                                                        GTFSstops$stop_lat),
                                    data = GTFSstops,
                                    proj4string = crs1)
    s <- sp::spTransform(s, crs2)
  }
  else
    {
    GTFSstops <- GTFSstops[!is.na(GTFSstops$stop_lon),]
    GTFSstops <- SIRItoGTFS::StopsForSIRI(SIRI = SIRI, stops = GTFSstops)
    proj = rgdal::make_EPSG()
    crs2 <- sp::CRS(proj$prj4[proj$code == epsg])
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(GTFSstops$stop_lon,
                                                        GTFSstops$stop_lat),
                                    data = GTFSstops,
                                    proj4string = crs1)
    s <- sp::spTransform(s, crs2)
  }
}
