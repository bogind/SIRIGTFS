#' @name stopstoSP
#' @title Projects the GTFS stops table to a SpatialPointsDataFrame
#' @description Projects a subset of the GTFS stops table to a \code{\link[sp]{SpatialPointsDataFrame}} with the selected projection.
#' @param GTFSstops. A subset of the GTFS stops table, should be the product of \code{\link{StopsForSIRI}}.
#' @param epsg a coordinate system code ( can be extracted using "make_EPSG" and searching the table).
#' @param useSIRI logical. use a SIRI data frame as reference?
#' @param SIRI if useSIRI is TRUE, which SIRI data frame to use?
#' @details
#' Requires the product of \code{\link{StopsForSIRI}},
#' is interchangeable with \code{\link{stopstoILTM}} for Israel's TM grid.
#' the function is a part of STG and should not be used on it's own.
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} with stops relevant to SIRI's real-time data.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{stopstoILTM}}, \code{\link{StopsForSIRI}}
#' @keywords misc internal
#' @importFrom sp CRS SpatialPointsDataFrame spTransform
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom rgdal make_EPSG


stopstoSP <- function(GTFSstops.,epsg ,useSIRI = FALSE, SIRI = NULL){

  if(useSIRI == FALSE & is.null(SIRI)){
    GTFSstops. <- GTFSstops.[!is.na(GTFSstops.$stop_lon),]
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
    GTFSstops <- StopsForSIRI(SIRI = SIRI, stops = GTFSstops)
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
