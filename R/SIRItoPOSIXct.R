#' @name SIRItoPOSIXct
#' @title convert SIRI date time columns to POSIXct
#' @description
#' For internal use in organizeSIRIdf, requires one column from a subset of the entire SIRI dataframe
#' @param column the column to reshape into POSIXct
#' @param round logical. round to nearest minute?
#' @return the same SIRI column with readable POSIXct values
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}, \code{\link{organizeSIRIdf}}
#' @keywords misc internal
#' @importFrom reshape2 colsplit
#' @export


SIRItoPOSIXct <- function(column, round = FALSE){

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
