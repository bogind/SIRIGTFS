#' @name check_outlier2
#' @title Secondary outliers test
#' @description
#' Requires the product of \link[SIRItoGTFS]{SIRIKeepClosestStop}
#' Checks if there are sufficient observations in the data to analyse the SIRI data.
#' @param fullans The product of a join between the SIRI data frame after \code{\link{SIRIKeepClosestStop}} and the stop times data frame after \code{\link{organizeStopTimes}}
#' @return A \code{\link[base]{data.frame}} containing only the observations which had 25% of the trip or more
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{SIRIKeepClosestStop}}
#' @keywords misc internal
#' @export
check_outlier2 <- function(fullans){
  for(trip in unique(fullans$trip_id)){
    testdf <- fullans[fullans$trip_id == trip,]
    maxstop <- max(fullans$stop_sequence)
    obs <- nrow(testdf)
    if(obs/maxstop < 0.25){
      fullans$outlier[fullans$trip_id == trip] = 2
    }
    else{
      fullans$outlier[fullans$trip_id == trip] = 0
    }
  }
  fullans
}
