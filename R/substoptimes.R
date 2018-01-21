#' @export

substoptimes <- function(SIRIdf,
                         GTFSstop_times. = GTFSstop_times,
                         GTFSroutes. = GTFSroutes,
                         GTFStrips. = GTFStrips,
                         GTFScalendar. = GTFScalendar){

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
