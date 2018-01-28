#' @name addtripnum
#' @title Add a trip number
#' @description
#' Requires the final product (ans2)
#' adds a unique trip number
#' @param ans2 A \code{\link[base]{data.frame}}. should be a finished SIRI to GTFS table, after \code{\link{check_outlier2}}
#' @details
#' Adds a unique trip number during the day for each trip recorded in SIRI,
#' the function is a part of STG and should not be used on it's own.
#' @return
#' A \code{\link[base]{data.frame}} containing the final result of \code{\link{STG}} with a trip number column added.
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link{STG}}
#' @keywords misc internal


addtripnum <- function(ans2){

  tripnum <- 1
  tripnumvec <- vector()

  ans2 <- ans2[base::order(ans2$OriginAimedDepartureTime,ans2$stop_sequence),]
  ans2 <- ans2[!is.na(ans2$OriginAimedDepartureTime),]

  for(ora in base::unique(ans2$OriginAimedDepartureTime[!is.na(ans2$OriginAimedDepartureTime)])){

    if(length(ans2$OriginAimedDepartureTime[ans2$OriginAimedDepartureTime[!is.na(ans2$OriginAimedDepartureTime)] == ora]) > 0 ){

      tripnumvec <-c(tripnumvec, rep_len(tripnum,length.out = length(ans2$OriginAimedDepartureTime[ans2$OriginAimedDepartureTime[!is.na(ans2$OriginAimedDepartureTime)] == ora]))  )

      tripnum <- tripnum+1
    }else{

      tripnum <- tripnum+1
    }
  }
  tripnumvec <- tripnumvec
  ans2 <- cbind(ans2,tripnumvec)
}
