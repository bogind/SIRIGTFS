lowmemST2POSIX <- function(column, SIRIref){
  s1 <- column
  s2 <- as.Date(as.POSIXct(SIRIref$RecordedAtTime[1:length(column)]))
  s2 <- as.character(s2)
  s2 <- rep(s2[1],len = length(s1))
  s3 <- s2[1:(length(s2)/2+1)]
  s4 <- s2[length(s3):length(s2)]
  s5 <- s1[1:(length(s1)/2+1)]
  s6 <- s1[(length(s1)/2+1):length(s1)]

  a1 <- paste0(s3, " ", s5)
  a2 <- paste0(s4, " ", s6)
  a3 <- (rbind(a1,a2))
  a3 <- a3[1:(length(a3)-1)]
  Y <- as.POSIXct(strptime(a3,format = "%Y-%m-%d %H:%M:%S"))
  column <- Y
  column <- column[-length(column)]
}
