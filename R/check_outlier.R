#' @name check_outlier
#' @title Check for outliers in a SIRI SpatialPointsDataFrame
#' @description
#' Add an outlier column to the SIRI data,
#' Requires the SIRI SpatialPointsDataFrame from \code{\link{SIRItoSP}}
#' @param spSIRI A \code{\link[sp]{SpatialPointsDataFrame}}, containing SIRI data.
#' should be used after \code{\link{SIRItoILTM}} or \code{\link{SIRItoSP}}
#' @details
#' Adds an outlier column to the SIRI data,
#' the function is a part of STG and should not be used on it's own.
#' @return
#' A \code{\link[sp]{SpatialPointsDataFrame}} with an extra column representing the outliers
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Spatial and Temporal Estimation of the Service Reliability of Public Transportation Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link[SIRItoGTFS]{STG}}, \code{\link[SIRItoGTFS]{SIRItoILTM}},
#' \code{\link[SIRItoGTFS]{SIRItoSP}}, \code{\link[sp]{SpatialPointsDataFrame}}
#' @keywords misc internal
#' @importFrom rgeos gConvexHull gBuffer gWithin
#' @importClassesFrom sp SpatialPointsDataFrame



check_outlier <- function(spSIRI){
  testsp <- spSIRI
  for(trip in unique(testsp@data$trip_id)){
    tested <- testsp[testsp@data$trip_id == trip,]
  siridfch <- rgeos::gConvexHull(tested)

  if(!is.null(siridfch)){
    cent1 <- rgeos::gCentroid(siridfch)
    centbuffer <- rgeos::gBuffer(cent1, width = 75)
    inlier <- rgeos::gWithin(tested, centbuffer)
    short <- (nrow(tested@data) <= 10)
    first <- min(tested@data$RecordedAtTime)
    last <- max(tested@data$RecordedAtTime)
    delta <- as.numeric(difftime(last,first,units = "mins"))
    if(inlier & short & delta <= 30){
      testsp@data$outlier[testsp@data$trip_id == trip] = 1
    }else{
      testsp@data$outlier[testsp@data$trip_id == trip] = 0
      }
    }
  }
  testsp
}
