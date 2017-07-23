StopTimes2POSIXct <- function(column, SIRIref){
  d <- column
  time <- as.character(d)
  date <- rep_len(as.character(as.Date(as.POSIXct(SIRIref$RecordedAtTime[1]))),length(time))
  X <- paste(as.character(date)," ", as.character(time))
  Y <- as.POSIXct(strptime(X,format = "%Y-%m-%d %H:%M:%S"))
  Y
}
