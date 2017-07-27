organizeStopTimes <- function(Stimes, SIRIdf3){
  s2 <- Stimes
  x <- unique(s2$trip_id)
  y <- s2$arrival_time[s2$stop_sequence == 1]
  xx <- as.data.frame(table(s2$trip_id))
  xx$Var2 <- y
  depart_firststr <- rep(xx$Var2,xx$Freq)
  if(length(depart_firststr) > 0){
    s3 <- cbind(s2, depart_firststr)
    s3$depart_firststr <- as.character(s3$depart_firststr)
    s3$depart_first <- SIRItoGTFS::StopTimes2POSIXct(s3$depart_firststr,SIRIdf3)
    s3$key <- paste(s3$depart_first," ", s3$stop_id)
    s3$arrival_time <- SIRItoGTFS::StopTimes2POSIXct(s3$arrival_time,SIRIdf3)
    s3$departure_time <- SIRItoGTFS::StopTimes2POSIXct(s3$departure_time,SIRIdf3)
    s3 <- s3[!is.na(s3$arrival_time),]
    s3
  }else{
    print('failed to subset stop times')
  }
}
