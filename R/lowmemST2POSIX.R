#' @name lowmemST2POSIX
#' @title convert Stop_times columns to POSIXct on low memory machines
#' @description
#' A possible replacement function for \code{\link{StopTimes2POSIXct}}.
#' Is not used by default.
#' used to convert Date and Time columns in the GTFS Stop_times table to a conformable POSIXct.
#' @param column A \code{\link[base]{vector}} to convert, should be a column in the GTFS Stop_Times table.
#' @param SIRIref A SIRI reference table from which to get the date.
#' @details use of this function requires editing both the STG function and \code{\link{organizeSIRIdf}} within it to replace all \code{\link{StopTimes2POSIXct}} to lowmemST2POSIX
#' @return Returns the column as a POSIXct \code{\link[base]{vector}}
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of Public Transportation Service Reliability Using Big Data and Open Source Tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{StopTimes2POSIXct}}
#' @keywords misc internal

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
