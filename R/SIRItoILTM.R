SIRItoILTM <- function(SIRIdf, epsg = NULL){
  if(is.null(epsg) == TRUE){
    require(sp)
    require(rgdal)
    SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
    proj = make_EPSG()
    israelTM <- CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(as.numeric(as.character(SIRIdf$Longitude)),as.numeric(as.character(SIRIdf$Latitude))),data = SIRIdf,proj4string = crs1)
    s <- spTransform(s, israelTM)
  } else{
    require(sp)
    require(rgdal)
    SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
    proj = make_EPSG()
    crs2 <- CRS(proj$prj4[proj$code == epsg])
    crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- SpatialPointsDataFrame(coords = data.frame(as.numeric(as.character(SIRIdf$Longitude)),as.numeric(as.character(SIRIdf$Latitude))),data = SIRIdf,proj4string = crs1)
    s <- spTransform(s, crs2)
  }
}
