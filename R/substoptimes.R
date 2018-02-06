#' @name substoptimes
#' @title Subset the stop_times table
#' @description Creates a simple subset of the GTFS to reference the SIRI data frame
#' @param SIRIdf A SIRI data.frame to use as reference
#' @param GTFSstop_times. The Stop times table from the GTFS, default name is GTFSstop_times
#' @param GTFSroutes. The Routes table from the GTFS, default name is GTFSroutes
#' @param GTFStrips. The trips table from the GTFS, default name is GTFStrips
#' @param GTFScalendar. The Calendar table from the GTFS, default name is TFScalendar
#' @details
#' creates a smaller subset of the GTFS stop_times table to use in further analysis of the SIRI data.
#' the function is a part of STG and should not be used on it's own.
#' @return
#' A \code{\link[base]{data.frame}} with the exact same columns as the GTFSstop_times table, with only the rows relevant to the SIRI data present
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of Public Transportation Service Reliability Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}
#' @keywords misc internal


substoptimes <- function(SIRIdf,
                         GTFSstop_times.,
                         GTFSroutes.,
                         GTFStrips.,
                         GTFScalendar.){

  trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf$LineRef,]

  cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]

  week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
  colnames(cal)[2:8] <- week
  cal[,9:10] <- sapply(cal[,9:10], as.character)
  cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
  cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")

  cal <- cal[cal$start_date <= as.Date(SIRIdf$RecordedAtTime[1])& as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date,]
  if(cal$start_date[1] <= as.Date(SIRIdf$RecordedAtTime[1]) & as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date[1] & NROW(cal) >= 1){

    c1 <- cal[any(weekdays(SIRIdf$RecordedAtTime[1]) == colnames(cal)[2:8]),]

    if(class(c1) == "data.frame"){
      t1 <- trips[trips$service_id %in% c1$service_id,]
      st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id,]
      st
    }else{
      c2 <- cal[c1 == 1,]
      t1 <- trips[trips$service_id %in% c2$service_id,]
      st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id,]
      st
    }
  }
  else{
    print("SIRI does not match GTFS dates or day of week")
  }
}
