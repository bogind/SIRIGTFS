#' @name STG
#' @title Wrapper function for the SIRItoGTFS library
#' @description
#' Performs a comparison between a SIRI data.frame and GTFS tables,
#' requires the SIRI table  as well as the minimal GTFS tables to be in the environment.
#' should be used after \code{\link{readGTFS}}.
#' @param SIRIDF A \code{\link[base]{data.frame}} containing SIRI protocol data downloaded from a public transportation authority.
#' @param GTFSstops. A GTFS stops table, best load into environment with \code{\link{readGTFS}}
#' @param GTFSagency. A GTFS agency table, best load into environment with \code{\link{readGTFS}}
#' @param GTFScalendar. A GTFS calendar table, best load into environment with \code{\link{readGTFS}}
#' @param GTFSroutes. A GTFS routes table, best load into environment with \code{\link{readGTFS}}
#' @param GTFSstop_times. A GTFS stop_times table, best load into environment with \code{\link{readGTFS}}
#' @param GTFStrips. A GTFS trips table, best load into environment with \code{\link{readGTFS}}
#' @param linerefs Optional, a numeric vector of GTFS route_id numbers to process. if not used all route_id's in the SIRIDF provided will be used.
#' @param epsg The EPSG code for the projection to be used.
#' @details
#' The function provides an "easy to use" wrapper for users unfamiliar with the functions in **SIRItoGTFS**.
#' It should be used after a SIRI table has been read into R's environment along with GTFS tables who have a corresponding date.
#' it is best used after \code{\link{readGTFS}}.
#' the SIRI table used should have the minimal columns:
#' "RecordedAtTime", "MonitoringRef", "LineRef", "DirectionRef", "PublishedLineName",
#' "OperatorRef", "DestinationRef", "OriginAimedDepartureTime", "Longitude",
#' "Latitude", "VehicleRef", "StopPointRef" & "ExpectedArrivalTime".
#' The output table will contain a time and distance comparison between the schedule provided in the GTFS tables and the real-time data provided
#' with the SIRI table.
#' @return A \code{\link[base]{data.frame}} containing a comparison between a public transportation mode's schedule and real-time data.
#' @references
#' Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Spatial and Temporal Estimation of the Service Reliability of Public Transportation Using Big Data and Open Source Tools}
#' @seealso \code{readGTFS}
#' @examples
#' require(SIRItoGTFS)
#' require(data.table)
#' # use the sample SIRI data included with the package
#' data("sirisample")
#' SIRIsample$Longitude = as.numeric(SIRIsample$Longitude)
#' SIRIsample$Latitude = as.numeric(SIRIsample$Latitude)
#' # load your own GTFS data with `readGTFS()`
#' # or use the subset of GTFS data conformable to the SIRI sample, also included in the package
#' data("GTFSstops")
#' data("GTFSstop_times")
#' data("GTFScalendar")
#' data("GTFStrips")
#' data("GTFSagency")
#' data("GTFSroutes")
#' busesDF = STG(SIRIsample,
#'              GTFSstops. = GTFSstops,
#'              GTFSagency. = GTFSagency,
#'              GTFScalendar. = GTFScalendar,
#'              GTFSroutes. = GTFSroutes,
#'              GTFSstop_times. = GTFSstop_times,
#'              GTFStrips. = GTFStrips,
#'              linerefs = unique(SIRIsample$LineRef[1]))
#'
#'
#' @keywords package spatial
#' @importFrom data.table rbindlist
#' @importFrom dplyr right_join
#' @import sp
#' @import rgdal
#' @import rgeos
#' @export


STG = function(SIRIDF,
                GTFSstops.,
                GTFSagency.,
                GTFScalendar.,
                GTFSroutes.,
                GTFSstop_times.,
                GTFStrips.,
                linerefs = NULL,
                epsg = 2039){


  w <- 1
  o <- 1
  listallbuses <- list()
  outliers <- NULL
  start <- Sys.time()



  print("Strating")
  for(lineref in linerefs){
    # SIRIdf
    looptime <- Sys.time()

    SIRIdf2 <- SubsetSIRI(SIRIDF, lineref)

    # this part will organize it and add a unique key
    # it takes some time...

    SIRIdf3 <- organizeSIRIdf(SIRIdf2, noduplicates = TRUE, round = FALSE,
                              GTFStrips., GTFScalendar., GTFSstop_times.)


    StimesforSIRI <- substoptimes(SIRIdf3, GTFSstop_times., GTFSroutes., GTFStrips. ,GTFScalendar.)


    if(NROW(StimesforSIRI$trip_id) < 1){
      print(paste("failed number: ", w, " in subset stop times"))
      w <- w+1
    }else{

      # organizeStopTimes takes the output of substoptimes and makes it ready for
      # comparison against the SIRI data frame

      Stimes2 <- organizeStopTimes(Stimes = StimesforSIRI, SIRIdf3. = SIRIdf3)


      # and this part will remove duplicates
      # to check this does not need to change the DF
      # SIRIdf <- SIRIdf[!duplicated(SIRIdf$key),]

      #Only for one line... this will not work for multiple lines
      SIRIstops <- StopsForSIRI(SIRI = SIRIdf3,stops = GTFSstops., trips = GTFStrips., stop_times = GTFSstop_times.) # DF of staions per line


      if(length(SIRIdf3$Longitude) == length(SIRIdf3$Longitude[is.na(SIRIdf3$Longitude)])){

        print(paste("failed number: ", w))
        w <- w+1

      }else{


        # for a generic version you can use SIRItoSP with use of an EPSG code, and
        spSIRI <- SIRItoSP(SIRIdf3,epsg) # change siriDF to point with ITM

        if(NROW(spSIRI[!is.na(spSIRI@data$trip_id),]) > 1){
          spSIRI <- spSIRI[!is.na(spSIRI@data$trip_id),]
          # find outliers
          spSIRI <- check_outlier(spSIRI)

        }else{
          spSIRI <- spSIRI[!is.na(spSIRI@data$OriginAimedDepartureTime),]

          spSIRI@data$outlier = 3

        }
        # spSIRI2 <- SIRItoSP(SIRIdf, 2039) # change siriDF to point with selected EPSG CRS
        spstops <- stopstoSP(SIRIstops,epsg) # change pointsDF to point with ITM

        # spstops2 <- stopstoSP(SIRIstops, 2039) # change pointsDF to point with selected EPSG CRS


        # nearest stop returns a SpatialPointsDataFrame object
        # if you want it to save to dataframe use the last row (SIRIdf2 <- spSIRI@data)
        spSIRI <- NearestStop(spSIRI,spstops)

        SIRIdf4 <- spSIRI@data


        # subsets the data frame further, leaving only the colsest call, per stop, per trip

        SIRIdf5 <- SIRIKeepClosestStop(SIRIdf4)

        # check what is the range of the times selected


        # both these actions did not require specific functions, they join the SIRI
        # data to it's rellevant GTFSstop_times data and creates a time difference column
        # which is used to check the amount of time the bus was early/late per stop.

        fullans <- right_join(SIRIdf5,Stimes2, by = c("key3" = "key", "trip_id" = "trip_id"))
        fullans <- check_outlier2(fullans)

        length(fullans$arrival_time[is.na(fullans$arrival_time)]) # the join causes quite a lot of NA's
        # but comparison to the number of rellevant obsevations shows that is missing data from SIRI

        fullans$timediff <- as.numeric(difftime(fullans$RecordedAtTime,fullans$arrival_time, units = "mins"))


        ans2 <- fullans[,c("RecordedAtTime","arrival_time", "timediff", "distance", "key3", "stop_code","stop_sequence","stop_lon","stop_lat","OriginAimedDepartureTime", "trip_id", "outlier" )]
        # write.csv(ans2, file = "C:\\Users\\Dror Bogin\\Desktop\\University\\Geogeraphy\\Seminar\\test\\line5ScG.csv")

        # checks how many observations you currently have
        print(paste("lineref no",w,"had",length(ans2$timediff[!is.na(ans2$timediff)]), "observations"))
        ans2$lineref <- rep_len(lineref,length(ans2$RecordedAtTime))
        if(length(ans2$RecordedAtTime[!is.na(ans2$RecordedAtTime)]) > 0){
          ans2 <- addtripnum(ans2)
          listallbuses[[w]] <- ans2
          print(paste("finished number: ", w))
          w <- w+1
        }
        else{
          print(paste("failed number: ", w))
          w <- w+1}
      }
    }
    end <- Sys.time()

    print(end-looptime)

    if(w >= length(linerefs)+1){
      print(paste("Finished All Bus lines in: ", end-start))}
    buses <- rbindlist(listallbuses, fill = TRUE)
    buses <- buses[!is.na(buses$timediff),]
    return(buses)

  }


}

