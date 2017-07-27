
SIRItoPOSIXct <- function(column, round = FALSE){
  require(reshape2, quietly = TRUE)

  if(round == TRUE){
    X <- reshape2::colsplit(column, "T", c("date","time"))
    X$time <- as.character(X$time)
    Y <- reshape2::colsplit(X$time, "000", c("time","uselss"))
    Y$time <- as.character(Y$time)
    Y$time <- substr(Y$time,1,nchar(Y$time)-1)
    X$time <- Y$time
    X$col <- paste0(as.character(X$date)," ", as.character(X$time))
    X$col <- as.POSIXct(strptime(X$col,format = "%Y-%m-%d %H:%M:%S"))

    column <- X$col}
  else {
    X <- reshape2::colsplit(column, "T", c("date","time"))
    X$time <- as.character(X$time)
    Y <- reshape2::colsplit(X$time, "000", c("time","uselss"))
    Y$time <- as.character(Y$time)
    Y$time <- substr(Y$time,1,nchar(Y$time)-1)
    X$time <- Y$time
    X$col <- paste0(as.character(X$date)," ", as.character(X$time))
    X$col <- as.POSIXct(strptime(X$col,format = "%Y-%m-%d %H:%M:%S"))
    column <- X$col
  }
}
