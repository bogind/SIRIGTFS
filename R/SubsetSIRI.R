#' @name SubsetSIRI
#' @title Subset your SIRI dataset to only one Route
#' @description A simple subset of the full SIRI dataframe to one selected bus route(lineref)
#' @param SIRIdf original SIRI data frame
#' @param lineref the lineref/route_id to be used.
#' @return A smaller \code{\link[base]{data.frame}} of SIRI data only containing the selected lineref's observations.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}
#' @keywords misc internal
#' @export

SubsetSIRI <- function(SIRIdf, lineref){
  subdf <- SIRIdf[SIRIdf$LineRef == lineref,]
  subdf
}

