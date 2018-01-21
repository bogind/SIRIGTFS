#' @export


StopsForSIRI <- function(SIRI, stops = GTFSstops){
  if(length(unique(SIRI$LineRef))>1){
    print("ERROR: SIRI file contains more then one LineRef, there should only be 1 unique LineRef")
  }else{
    stops[which(stops$stop_code %in% SIRI$StopPointRef, arr.ind = TRUE),]
  }
}
