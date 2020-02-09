#' @name organizeStopTimes
#' @title Organizes the GTFS Stop_times table
#' @description
#' Requires the product of \code{\link{substoptimes}}.
#' Prepares the subset of the GTFS stop_times table for comparison with SIRI's real-time data.
#' @param Stimes The product of \code{\link{substoptimes}}, a GTFS stop_times \code{\link[base]{data.frame}} subset.
#' @param SIRIdf3. A SIRI \code{\link[base]{data.frame}} after \code{\link{organizeSIRIdf}}.
#' @return A \code{\link[base]{data.frame}} containing columns which can be compared to SIRI's date and time columns.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Spatial and Temporal Estimation of the Service Reliability of Public Transportation Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{substoptimes}}, \code{\link{organizeSIRIdf}}
#' @keywords misc internal


organizeStopTimes <- function(Stimes, SIRIdf3.){
  s2 <- Stimes
  x <- unique(s2$trip_id)
  y <- as.character(s2$arrival_time[s2$stop_sequence == 1])
  xx <- as.data.frame(table(s2$trip_id))
  xx$Var2 <- y
  depart_firststr <- rep(xx$Var2,xx$Freq)
  if(length(depart_firststr) > 0){
    s3 <- cbind(s2, depart_firststr)
    s3$depart_firststr <- as.character(s3$depart_firststr)
    s3$depart_first <- StopTimes2POSIXct(s3$depart_firststr,SIRIdf3.)
    s3$key <- paste(s3$depart_first," ", s3$stop_id)
    s3$arrival_time <- StopTimes2POSIXct(s3$arrival_time,SIRIdf3.)
    s3$departure_time <- StopTimes2POSIXct(s3$departure_time,SIRIdf3.)
    s3 <- s3[!is.na(s3$arrival_time),]
    return(s3)
  }else{
    print('failed to subset stop times')
  }
}
