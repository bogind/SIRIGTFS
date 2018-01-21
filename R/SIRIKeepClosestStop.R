#' base::duplicated added since duplicated exists in data.table as well
#'
#'  @export

SIRIKeepClosestStop <- function(SIRIdf){
  a <- SIRIdf[order(SIRIdf$OriginAimedDepartureTime ,SIRIdf$stop_code, SIRIdf$distance),]
  a$key2 <- paste(a$OriginAimedDepartureTime," ", a$stop_code)
  a$key3 <- paste(a$OriginAimedDepartureTime," ", a$stop_id)
  b <- a[!base::duplicated(a$key2),]
  b
}
