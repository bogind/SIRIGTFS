#' @export SIRIKeepClosestStop

SIRIKeepClosestStop <- function(SIRIdf){
  a <- SIRIdf[order(SIRIdf$OriginAimedDepartureTime ,SIRIdf$stop_code, SIRIdf$distance),]
  a$key2 <- paste(a$OriginAimedDepartureTime," ", a$stop_code)
  a$key3 <- paste(a$OriginAimedDepartureTime," ", a$stop_id)
  b <- a[!duplicated(a$key2),]
  b
}
