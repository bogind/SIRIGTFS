#' @name stopstoILTM
#' @title Projects the GTFS stops table to Israel's TM grid
#' @description
#' Projects a subset of the GTFS stops table to a \code{\link[sp]{SpatialPointsDataFrame}} With Israel's TM projection (epsg code 2039).
#' @param GTFSstops. A subset of the GTFS stops table, should be the product of \code{\link{StopsForSIRI}}
#' @param useSIRI logical. use a SIRI data frame as reference?
#' @param SIRI if useSIRI is TRUE, which SIRI data frame to use?
#' @details
#' Requires the product of \code{\link{StopsForSIRI}},
#' is interchangeable with \code{\link{stopstoSP}}.
#' the function is a part of STG and should not be used on it's own.
#' @return A \code{\link[sp]{SpatialPointsDataFrame}} with stops relevant to SIRI's real-time data
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{stopstoSP}}, \code{\link{StopsForSIRI}}
#' @keywords misc internal
#' @importFrom sp CRS SpatialPointsDataFrame spTransform
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom rgdal make_EPSG
#' @export

stopstoILTM <- function(GTFSstops., useSIRI = FALSE, SIRI = NULL){

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
