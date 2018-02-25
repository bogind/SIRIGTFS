#' @name SIRItoSP
#' @title Projects a SIRI data frame to a spatial points data frame
#' @description Projects a subset of the SIRI data frame to a \code{\link[sp]{SpatialPointsDataFrame}} with the selected projection.
#' @param SIRIdf the data frame to project
#' @param epsg a coordinate reference system code (can be extracted using \code{\link[rgdal]{make_EPSG}} and searching the table).
#' @details
#' Requires the product of \code{\link{organizeSIRIdf}},
#' is interchangeable with \link[SIRItoGTFS]{SIRItoILTM}.
#' the function is a part of STG and should not be used on it's own.
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} with SIRI's real-time data
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Spatial and Temporal Estimation of the Service Reliability of Public Transportation Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{SIRItoILTM}}, \code{\link[sp]{SpatialPointsDataFrame}}, \code{\link{organizeSIRIdf}}
#' @keywords misc internal spatial
#' @importFrom sp CRS SpatialPointsDataFrame spTransform
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom rgdal make_EPSG


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
