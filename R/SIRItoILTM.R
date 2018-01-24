#' @name SIRItoILTM
#' @importFrom sp CRS spTransform SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom rgdal make_EPSG
#' @title Projects a SIRI data frame to a spatial points object with Israel's TM grid
#' @description
#' Projects a subset of the SIRI data frame to a \code{\link[sp]{SpatialPointsDataFrame}} With Israel's TM projection (epsg 2039).
#' Requires the product of \code{\link{organizeSIRIdf}}
#' @param SIRIdf A SIRI df after organizeSIRIdf (SIRIdf3)
#' @param epsg a coordinate system code ( can be extracted using "make_EPSG" and searching the table ),
#' default is NULL with Israel's TM Grid (epsg 2039)
#' @details
#' Requires the product of \code{\link{organizeSIRIdf}},
#' is interchangeable with \link[SIRItoGTFS]{SIRItoSP}.
#' the function is a part of STG and should not be used on it's own.
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} with SIRI's real-time data
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{SIRItoSP}}, \code{\link{organizeSIRIdf}}
#' @keywords misc internal spatial
#' @export SIRItoILTM


SIRItoILTM <- function(SIRIdf, epsg = NULL){
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
