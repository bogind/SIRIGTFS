SIRItoSP <- function(SIRIdf, epsg){
  require(sp)
  require(rgdal)
  SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
  proj = make_EPSG()
  crs2 <- CRS(proj$prj4[proj$code == epsg])
  crs1 <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  s <- SpatialPointsDataFrame(coords = data.frame(SIRIdf$Longitude,SIRIdf$Latitude),data = SIRIdf,proj4string = crs1)
  s <- spTransform(s, crs2)
}
