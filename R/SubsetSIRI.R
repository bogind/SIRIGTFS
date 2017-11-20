#' @export SubsetSIRI

SubsetSIRI <- function(SIRIdf, lineref){
  subdf <- SIRIdf[SIRIdf$LineRef == lineref,]
  subdf
}

