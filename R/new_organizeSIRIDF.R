#' @name organizeSIRIdf2
#' @title Organize the subset of the SIRI DataFrame
#' @importFrom reshape2 colsplit
#' @importFrom dplyr left_join
#' @importFrom lubridate day
#' @description
#' Requires the product of \code{\link{SubsetSIRI}}.
#' Prepares the subset of the SIRI data for comparison with the GTFS tables.
#' @param SIRIDF A SIRI \code{\link[base]{data.frame}}
#' @param noduplicates logical, default is FALSE
#' @param round logical, should POSIXct column be rounded to nearest minute. default is FALSE.
#' @param GTFStrips. The GTFS trips table to be used
#' @param GTFScalendar. The GTFS calendar table to be used
#' @param GTFSstop_times. The GTFS stop_times table to be used
#' @details
#' Prepares the subset of the SIRI data.frame for comparison to the GTFS schedule
#' the function is a part of STG and should not be used on it's own.
#' @seealso \code{\link[SIRItoGTFS]{STG}}
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Spatial and Temporal Estimation of the Service Reliability of Public Transportation Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @keywords misc internal

organizeSIRIDF <- function(SIRIdf2, noduplicates = FALSE, round = FALSE,
                           GTFStrips., GTFScalendar., GTFSstop_times.){

  # if(noduplicates == FALSE){

    time = as.POSIXct(gsub(":", "", SIRIdf2$OriginAimedDepartureTime),
                      "%Y-%m-%dT%H%M%OS%z", tz = Sys.timezone())
    SIRIdf2$OriginAimedDepartureTime <- time
    SIRIdf2$arrival_time = as.character(strftime(time[1], format="%H:%M:%S"))
    SIRIdf2 = SIRIdf2[!is.na(SIRIdf2$OriginAimedDepartureTime), ]
    if (length(unique(day(time))) > 1) {
      datet = data.frame(table(lubridate::date(SIRIdf2$OriginAimedDepartureTime),dnn = c("date")))
      print(paste("SIRI data frame contained ",
                  length(unique(day(time))),
                  " dates, the one with most values was used"  ))
      SIRIdf2 = SIRIdf2[lubridate::date(SIRIdf2$OriginAimedDepartureTime) ==
                           lubridate::date(datet$date[which.max(datet[,2])]),]
    }

    SIRIdf2$RecordedAtTime <- as.POSIXct(gsub(":", "", SIRIdf2$RecordedAtTime),
                                         "%Y-%m-%dT%H%M%OS%z", tz = Sys.timezone())
    SIRIdf2$ExpectedArrivalTime <- as.POSIXct(gsub(":", "", SIRIdf2$ExpectedArrivalTime),
                                              "%Y-%m-%dT%H%M%OS%z", tz = Sys.timezone())
    SIRIdf2$AimedArrivalTime <- as.POSIXct(gsub(":", "", SIRIdf2$AimedArrivalTime),
                                           "%Y-%m-%dT%H%M%OS%z", tz = Sys.timezone())
    if(length(unique(day(SIRIdf2$RecordedAtTime))) > 1){
      datet = data.frame(table(lubridate::date(SIRIdf2$RecordedAtTime),dnn = c("date")))
      SIRIdf2 = SIRIdf2[lubridate::date(SIRIdf2$RecordedAtTime) ==
                          lubridate::date(datet$date[which.max(datet[,2])]),]
    }
    SIRIdf2$key <- paste(SIRIdf2$request_id, SIRIdf2$OriginAimedDepartureTime, SIRIdf2$VehicleRef, sep = " ; ")
    SIRIdf2$BUS_XY <- ifelse(is.na(SIRIdf2$Longitude) |
                              is.na(SIRIdf2$Latitude), NA, paste(SIRIdf2$Longitude,
                                                                SIRIdf2$Latitude, sep = " , "))

    trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf2$LineRef,]
    cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]
    week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
    colnames(cal)[2:8] <- week
    cal = as.data.frame(cal)
    cal[,9:10] <- sapply(cal[,9:10], as.character)
    cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
    cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")
    cal <- cal[cal$start_date <= as.Date(SIRIdf2$RecordedAtTime[1]) &
                 as.Date(SIRIdf2$RecordedAtTime[1]) <= cal$end_date,]
    tryCatch({
      if(cal$start_date[1] <= as.Date(SIRIdf2$RecordedAtTime[1]) &
         as.Date(SIRIdf2$RecordedAtTime[1]) <= cal$end_date[1] &
         NROW(cal) >=1){
          weekday = colnames(cal)[weekdays(SIRIdf2$RecordedAtTime[2]) == colnames(cal)]
          cols = c("service_id",weekday)
          c1 = cal[cal[,weekday] > 0,cols]
          if(class(c1) == "data.frame"){
            t1 <- trips[trips$service_id %in% c1$service_id,]
          }else{
            c2 <- cal[c1 == 1,]
            t1 <- trips[trips$service_id %in% c2$service_id,]
          }
          st1 <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id ,]
          st <- st1[st1$stop_sequence == 1,]
          if(nrow(st) < 1){
            stop("calendar did not match SIRI data")
          }
          st$arrival_time = as.character(st$arrival_time)
          ch <- unique(as.character(strftime(SIRIdf2$OriginAimedDepartureTime, "%H:%M:%S")))
          SIRIdf2 <- dplyr::left_join(SIRIdf2, st[,c("arrival_time","trip_id")], by = "arrival_time")
          SIRIdf2 <- SIRIdf2[order(SIRIdf2$request_id,SIRIdf2$OriginAimedDepartureTime ,SIRIdf2$VehicleRef,SIRIdf2$BUS_XY, rev(SIRIdf2$RecordedAtTime)),]
          SIRIdf2 <- SIRIdf2[!is.na(SIRIdf2$LineRef),]
          if(noduplicates){
            SIRIdf2 <- SIRIdf2[!duplicated(SIRIdf2$key),]
          }
          return(list(SIRIdf2,st1))
      }else{stop("SIRI date does not match the loaded GTFS")
      }
    }, error=function(e){})

}


