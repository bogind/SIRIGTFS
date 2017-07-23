addtripnum <- function(ans2){

  tripnum <- 1
  tripnumvec <- vector()

  ans2 <- ans2[order(ans2$OriginAimedDepartureTime,ans2$stop_sequence),]
  ans2 <- ans2[!is.na(ans2$OriginAimedDepartureTime),]

  for(ora in unique(ans2$OriginAimedDepartureTime[!is.na(ans2$OriginAimedDepartureTime)])){

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
