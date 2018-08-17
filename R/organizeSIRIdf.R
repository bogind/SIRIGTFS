#' @name organizeSIRIdf
#' @title Organize the subset of the SIRI DataFrame
#' @importFrom reshape2 colsplit
#' @importFrom dplyr left_join
#' @description
#' Requires the product of \code{\link{SubsetSIRI}}.
#' Prepares the subset of the SIRI data for comparison with the GTFS tables.
#' @param SIRIdf A SIRI \code{\link[base]{data.frame}}
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

organizeSIRIdf <- function(SIRIdf, noduplicates = FALSE, round = FALSE,
                           GTFStrips., GTFScalendar., GTFSstop_times.){

  if(noduplicates == FALSE){

    X <- reshape2::colsplit(SIRIdf$OriginAimedDepartureTime, "T", c("date","time"))
    X$time <- as.character(X$time)
    X$date <- as.POSIXct(X$date, "%Y-%m-%d", tz = Sys.timezone())
    X$weekday <- weekdays(X$date)
    X$date <- as.character(X$date)
    Y <- reshape2::colsplit(X$time, "000", c("time","uselss"))
    Y$time <- as.character(Y$time)
    Y$time <- substr(Y$time,1,nchar(Y$time)-1)
    X$time <- Y$time
    SIRIdf <- cbind(SIRIdf, X)
    SIRIdf <- SIRIdf[!is.na(SIRIdf$date),]

    if(length(unique(SIRIdf$date)) > 1){
      print(paste("SIRI data frame contained ", length(unique(SIRIdf$date)), " dates, the one with most values was used"  ))
      datet <- data.frame(table(SIRIdf$date))
      SIRIdf <- SIRIdf[SIRIdf$date == datet$Var1[which.max(datet$Freq)],]

      SIRIdf$RecordedAtTime <- SIRItoPOSIXct(SIRIdf$RecordedAtTime, round = FALSE)
      SIRIdf$OriginAimedDepartureTime <- SIRItoPOSIXct(SIRIdf$OriginAimedDepartureTime)
      SIRIdf$ExpectedArrivalTime <- SIRItoPOSIXct(SIRIdf$ExpectedArrivalTime)
      SIRIdf$AimedArrivalTime <- SIRItoPOSIXct(SIRIdf$AimedArrivalTime)
      SIRIdf$key <-paste(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime, SIRIdf$VehicleRef, sep = " ; ")
      SIRIdf$BUS_XY <- ifelse(is.na(SIRIdf$Longitude) | is.na(SIRIdf$Latitude), NA,  paste(SIRIdf$Longitude, SIRIdf$Latitude, sep = " , "))

      trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf$LineRef,]
      cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]
      week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
      colnames(cal)[2:8] <- week
      cal[,9:10] <- sapply(cal[,9:10], as.character)
      cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
      cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")
      cal <- cal[cal$start_date <= as.Date(SIRIdf$RecordedAtTime[1])& as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date,]
      if(cal$start_date[1] <= as.Date(SIRIdf$RecordedAtTime[1]) & as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date[1] & NROW(cal) >=1){
        c1 <- cal[,weekdays(SIRIdf$RecordedAtTime[1]) == colnames(cal)[2:8]]
        if(class(c1) == "data.frame"){
          t1 <- trips[trips$service_id %in% c1$service_id,]
        }else{
          c2 <- cal[c1 == 1,]
          t1 <- trips[trips$service_id %in% c2$service_id,]
        }
        st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id & GTFSstop_times.$stop_sequence == 1,]
        ch <- unique(as.character(strftime(SIRIdf$OriginAimedDepartureTime, "%H:%M:%S")))
        SIRIdf <- dplyr::left_join(SIRIdf, st, by = c("time" = "arrival_time"))
        SIRIdf <- SIRIdf[,-which(names(SIRIdf) %in% c("weekday", "time", "date", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "shape_dist_traveled"))]
        SIRIdf <- SIRIdf[order(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime ,SIRIdf$VehicleRef,SIRIdf$BUS_XY, rev(SIRIdf$RecordedAtTime)),]
        SIRIdf <- SIRIdf[!is.na(SIRIdf$LineRef),]
        SIRIdf
      }else{print("SIRI date does not match the loaded GTFS")}


    }else{
      SIRIdf$RecordedAtTime <- SIRItoPOSIXct(SIRIdf$RecordedAtTime, round = FALSE)
      SIRIdf$OriginAimedDepartureTime <- SIRItoPOSIXct(SIRIdf$OriginAimedDepartureTime)
      SIRIdf$ExpectedArrivalTime <- SIRItoPOSIXct(SIRIdf$ExpectedArrivalTime)
      SIRIdf$AimedArrivalTime <- SIRItoPOSIXct(SIRIdf$AimedArrivalTime)
      SIRIdf$key <-paste(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime, SIRIdf$VehicleRef, sep = " ; ")
      SIRIdf$BUS_XY <- ifelse(is.na(SIRIdf$Longitude) | is.na(SIRIdf$Latitude), NA,  paste(SIRIdf$Longitude, SIRIdf$Latitude, sep = " , "))

      trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf$LineRef,]
      cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]
      week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
      colnames(cal)[2:8] <- week
      cal[,9:10] <- sapply(cal[,9:10], as.character)
      cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
      cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")
      cal <- cal[cal$start_date <= as.Date(SIRIdf$RecordedAtTime[1])& as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date,]
      if(cal$start_date[1] <= as.Date(SIRIdf$RecordedAtTime[1]) & as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date[1] & NROW(cal) >=1){
        c1 <- cal[,weekdays(SIRIdf$RecordedAtTime[1]) == colnames(cal)[2:8]]
        if(class(c1) == "data.frame"){
          t1 <- trips[trips$service_id %in% c1$service_id,]
        }else{
          c2 <- cal[c1 == 1,]
          t1 <- trips[trips$service_id %in% c2$service_id,]
        }
        st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id & GTFSstop_times.$stop_sequence == 1,]
        ch <- unique(as.character(strftime(SIRIdf$OriginAimedDepartureTime, "%H:%M:%S")))
        SIRIdf <- dplyr::left_join(SIRIdf, st, by = c("time" = "arrival_time"))
        SIRIdf <- SIRIdf[,-which(names(SIRIdf) %in% c("weekday", "time", "date", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "shape_dist_traveled"))]
        SIRIdf <- SIRIdf[order(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime ,SIRIdf$VehicleRef,SIRIdf$BUS_XY, rev(SIRIdf$RecordedAtTime)),]
        SIRIdf <- SIRIdf[!is.na(SIRIdf$LineRef),]
        SIRIdf
      }else{print("SIRI date does not match the loaded GTFS")}

    }
  }
  else
  {
    X <- reshape2::colsplit(SIRIdf$OriginAimedDepartureTime, "T", c("date","time"))
    X$time <- as.character(X$time)
    X$date <- as.POSIXct(X$date, "%Y-%m-%d", tz = Sys.timezone())
    X$weekday <- weekdays(X$date)
    X$date <- as.character(X$date)
    Y <- reshape2::colsplit(X$time, "000", c("time","uselss"))
    Y$time <- as.character(Y$time)
    Y$time <- substr(Y$time,1,nchar(Y$time)-1)
    X$time <- Y$time
    SIRIdf <- cbind(SIRIdf, X)
    SIRIdf <- SIRIdf[!is.na(SIRIdf$date),]

    if(length(unique(SIRIdf$date)) > 1){
      print(paste("SIRI data frame contained ", length(unique(SIRIdf$date)), "dates, the one with most values was used"  ))
      datet <- data.frame(table(SIRIdf$date))
      SIRIdf <- SIRIdf[SIRIdf$date == datet$Var1[which.max(datet$Freq)],]


      SIRIdf$RecordedAtTime <- SIRItoPOSIXct(SIRIdf$RecordedAtTime, round = FALSE)
      SIRIdf$OriginAimedDepartureTime <- SIRItoPOSIXct(SIRIdf$OriginAimedDepartureTime)
      SIRIdf$ExpectedArrivalTime <- SIRItoPOSIXct(SIRIdf$ExpectedArrivalTime)
      SIRIdf$AimedArrivalTime <- SIRItoPOSIXct(SIRIdf$AimedArrivalTime)
      SIRIdf$key <-paste(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime, SIRIdf$VehicleRef, sep = " ; ")
      SIRIdf$BUS_XY <- ifelse(is.na(SIRIdf$Longitude) | is.na(SIRIdf$Latitude), NA,  paste(SIRIdf$Longitude, SIRIdf$Latitude, sep = " , "))

      trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf$LineRef,]
      cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]
      week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
      colnames(cal)[2:8] <- week
      cal[,9:10] <- sapply(cal[,9:10], as.character)
      cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
      cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")
      cal <- cal[cal$start_date <= as.Date(SIRIdf$RecordedAtTime[1])& as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date,]
      if(cal$start_date[1] <= as.Date(SIRIdf$RecordedAtTime[1]) & as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date[1] & NROW(cal) >=1 ){
        c1 <- cal[,weekdays(SIRIdf$RecordedAtTime[1]) == colnames(cal)]
        if(class(c1) == "data.frame"){
          t1 <- trips[trips$service_id %in% c1$service_id,]
        }else{
          c2 <- cal[c1 == 1,]
          t1 <- trips[trips$service_id %in% c2$service_id,]
        }
        st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id & GTFSstop_times.$stop_sequence == 1,]
        ch <- unique(as.character(strftime(SIRIdf$OriginAimedDepartureTime, "%H:%M:%S")))
        SIRIdf <- dplyr::left_join(SIRIdf, st, by = c("time" = "arrival_time"))

        SIRIdf <- SIRIdf[,-which(names(SIRIdf) %in% c("weekday", "time", "date", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "shape_dist_traveled"))]
        SIRIdf <- SIRIdf[order(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime ,SIRIdf$VehicleRef,SIRIdf$BUS_XY, rev(SIRIdf$RecordedAtTime)),]

        SIRIdf <- SIRIdf[!is.na(SIRIdf$LineRef),]
        SIRIdf
      }else{print("SIRI date does not match the loaded GTFS")}


    }else{
      SIRIdf$RecordedAtTime <- SIRItoPOSIXct(SIRIdf$RecordedAtTime, round = FALSE)
      SIRIdf$OriginAimedDepartureTime <- SIRItoPOSIXct(SIRIdf$OriginAimedDepartureTime)
      SIRIdf$ExpectedArrivalTime <- SIRItoPOSIXct(SIRIdf$ExpectedArrivalTime)
      SIRIdf$AimedArrivalTime <- SIRItoPOSIXct(SIRIdf$AimedArrivalTime)
      SIRIdf$key <-paste(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime, SIRIdf$VehicleRef, sep = " ; ")
      SIRIdf$BUS_XY <- ifelse(is.na(SIRIdf$Longitude) | is.na(SIRIdf$Latitude), NA,  paste(SIRIdf$Longitude, SIRIdf$Latitude, sep = " , "))

      trips <- GTFStrips.[GTFStrips.$route_id %in% SIRIdf$LineRef,]
      cal <- GTFScalendar.[GTFScalendar.$service_id %in% trips$service_id,]
      week <- c("Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday", "Saturday" )
      colnames(cal)[2:8] <- week
      cal[,9:10] <- sapply(cal[,9:10], as.character)
      cal[,9] <- as.Date(cal[,9], format = "%Y%m%d")
      cal[,10] <- as.Date(cal[,10], format = "%Y%m%d")
      cal <- cal[cal$start_date <= as.Date(SIRIdf$RecordedAtTime[1])& as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date,]

      if(cal$start_date[1] <= as.Date(SIRIdf$RecordedAtTime[1]) & as.Date(SIRIdf$RecordedAtTime[1]) <= cal$end_date[1] & NROW(cal) >=1){
        c1 <- cal[,weekdays(SIRIdf$RecordedAtTime[1]) == colnames(cal)]
        if(class(c1) == "data.frame"){
          t1 <- trips[trips$service_id %in% c1$service_id,]
        }else{
          c2 <- cal[c1 == 1,]
          t1 <- trips[trips$service_id %in% c2$service_id,]
        }
        st <- GTFSstop_times.[GTFSstop_times.$trip_id %in% t1$trip_id & GTFSstop_times.$stop_sequence == 1,]
        ch <- unique(as.character(strftime(SIRIdf$OriginAimedDepartureTime, "%H:%M:%S")))
        SIRIdf <- dplyr::left_join(SIRIdf, st, by = c("time" = "arrival_time"))
        SIRIdf <- SIRIdf[,-which(names(SIRIdf) %in% c("weekday", "time", "date", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "shape_dist_traveled"))]
        SIRIdf <- SIRIdf[order(SIRIdf$request_id,SIRIdf$OriginAimedDepartureTime ,SIRIdf$VehicleRef,SIRIdf$BUS_XY, rev(SIRIdf$RecordedAtTime)),]

        SIRIdf <- SIRIdf[!is.na(SIRIdf$LineRef),]
        SIRI <- SIRIdf[!duplicated(SIRIdf$key),]
        SIRI
      }else{print("SIRI date does not match the loaded GTFS")}

    }
  }
}


