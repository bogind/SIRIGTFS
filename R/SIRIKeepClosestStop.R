#' @name SIRIKeepClosestStop
#' @title Keep only the closest stop to each SIRI observation
#' @description
#' Removes duplicates from the SIRI data frame, and keep only the closest stop to each SIRI observation.
#' requires the data.frame from the product of \code{\link{NearestStop}}.
#' @param SIRIdf A SIRI df with a distance field (SIRIdf4)
#' @return A SIRI \code{\link[base]{data.frame}} with removed duplicates.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}
#' @keywords misc internal


SIRIKeepClosestStop <- function(SIRIdf){
  a <- SIRIdf[order(SIRIdf$OriginAimedDepartureTime ,SIRIdf$stop_code, SIRIdf$distance),]
  a$key2 <- paste(a$OriginAimedDepartureTime," ", a$stop_code)
  a$key3 <- paste(a$OriginAimedDepartureTime," ", a$stop_id)
  b <- a[!base::duplicated(a$key2),]
  b
}
