#' @name StopsForSIRI
#' @title subset stops for SIRIdf
#' @description
#' A simple subset of the GTFS stops table to the SIRI data being used in the current analysis.
#' requires the SIRI data.frame to be after \code{\link{organizeSIRIdf}} and the original GTFS stops table to be used.
#' @param SIRI a SIRIdf after organizeSIRIdf (SIRIdf3)
#' @param stops GTFSstops table
#' @return A subset of the GTFS stops table conforming to the bus route being evaluated.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}
#' @keywords misc internal



StopsForSIRI <- function(SIRI, stops){
  if(length(unique(SIRI$LineRef))>1){
    print("ERROR: SIRI file contains more then one LineRef, there should only be 1 unique LineRef")
  }else{
    stops[which(stops$stop_code %in% SIRI$StopPointRef, arr.ind = TRUE),]
  }
}
