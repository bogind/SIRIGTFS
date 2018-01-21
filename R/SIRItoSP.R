#' @importFrom sp CRS SpatialPointsDataFrame spTransform
#' @importFrom rgdal make_EPSG
#' @export SIRItoSP

SIRItoSP <- function(SIRIdf, epsg){

    SIRIdf <- SIRIdf[!is.na(SIRIdf$Longitude),]
  proj = rgdal::make_EPSG()
  crs2 <- sp::CRS(proj$prj4[proj$code == epsg])
  crs1 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  s <- sp::SpatialPointsDataFrame(coords = data.frame(SIRIdf$Longitude,
                                                      SIRIdf$Latitude),
                                  data = SIRIdf,
                                  proj4string = crs1)
  s <- sp::spTransform(s, crs2)
}
