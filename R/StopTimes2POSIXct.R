#' @name StopTimes2POSIXct
#' @title Converts a Stoptimes column to POSIXct
#' @description
#' Convert a date time column from the GTFS stop_times table to POSIXct for comparison with SIRI data.
#' used inside of organizeStoptimes
#' @param column the column to reshape
#' @param SIRIref a SIRI reference table from which to get the date
#' @details
#' The function is intended to work with a SIRI reference table containing only date.
#' the function is a part of \code{\link{organizeStopTimes}} inside \code{\link{STG}} and should not be used on it's own.
#' it is also interchangeable with \link{lowmemST2POSIX} for advanced user with machines low on memory.
#' @return A \code{\link[base]{vector}} of POSIXct values in "\%Y-\%m-\%d \%H:\%M:\%S" format.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}} \code{\link{organizeStopTimes}}, \code{\link{lowmemST2POSIX}}
#' @keywords misc internal
#' @export

StopTimes2POSIXct <- function(column, SIRIref){
  d <- column
  time <- as.character(d)
  date <- rep_len(as.character(as.Date(as.POSIXct(SIRIref$RecordedAtTime[1]))),length(time))
  X <- paste(as.character(date)," ", as.character(time))
  Y <- as.POSIXct(strptime(X,format = "%Y-%m-%d %H:%M:%S"))
  Y
}
