#' @importFrom sp CRS spTransform SpatialPointsDataFrame
#' @importFrom rgdal make_EPSG
#' @export SIRItoILTM


SIRItoILTM <- function(SIRIdf, epsg = NULL){
  require(sp, quietly = TRUE)
  require(rgdal, quietly = TRUE)
  if(is.null(epsg)){
    SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
    proj = rgdal::make_EPSG()
    israelTM <- sp::CRS("+proj=tmerc +lat_0=31.73439361111111 +lon_0=35.20451694444445 +k=1.0000067 +x_0=219529.584 +y_0=626907.39 +ellps=GRS80 +towgs84=-48,55,52,0,0,0,0 +no_defs")
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(as.numeric(as.character(SIRIdf$Longitude)),as.numeric(as.character(SIRIdf$Latitude))),data = SIRIdf,proj4string = crs1)
    s <- sp::spTransform(s, israelTM)
  } else{
    SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
    proj = rgdal::make_EPSG()
    crs2 <- sp::CRS(proj$prj4[proj$code == epsg])
    crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    s <- sp::SpatialPointsDataFrame(coords = data.frame(as.numeric(as.character(SIRIdf$Longitude)),as.numeric(as.character(SIRIdf$Latitude))),data = SIRIdf,proj4string = crs1)
    s <- sp::spTransform(s, crs2)
  }
}
