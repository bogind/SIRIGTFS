#' @export STG2


STG2 = function(SIRIDf,
                GTFSstops = GTFSstops,
                GTFSagency = GTFSagency,
                GTFSroutes = GTFSroutes,
                GTFSstop_times = GTFSstop_times,
                GTFStrips = GTFStrips,
                dfname = "busesStG",
                linerefs = NULL){

  require(tcltk,quietly = TRUE)
  require(data.table,quietly = TRUE)
  require(easycsv,quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(sp, quietly = TRUE)
  require(reshape2, quietly = TRUE)
  require(rgdal, quietly = TRUE)
  require(rgeos, quietly = TRUE)


  w <- 1
  o <- 1
  listallbuses <- list()
  outliers <- NULL
  start <- Sys.time()
  total = 11


  print("Strating")
  for(lineref in linerefs){
    # SIRIdf
    looptime <- Sys.time()
    pb <- tkProgressBar(title = paste("Line number ",w," out of ", length(linerefs)),
                        min = 0,
                        max = total,
                        initial=0,
                        width=400)

    pbi = 0

    SIRIdf2 <- SubsetSIRI(SIRIdf, lineref)
    print("created SIRIdf2")
    pbi = pbi+1
    setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                           "% done"))
    # this part will organize it and add a unique key
    # it takes some time...

    SIRIdf3 <- organizeSIRIdf(SIRIdf2, noduplicates = TRUE, round = FALSE)
    print("created SIRIdf3")
    pbi = pbi+1
    setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                           "% done"))

    StimesforSIRI <- substoptimes(SIRIdf3, GTFSstop_times = GTFSstop_times, GTFSroutes = GTFSroutes, GTFStrips = GTFStrips,GTFScalendar = GTFScalendar)
    pbi = pbi+1
    setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                           "% done"))
    print("created StimesforSIRI")

    if(NROW(StimesforSIRI$trip_id) < 1){
      print(paste("failed number: ", w, " in subset stop times"))
      w <- w+1
    }else{

      # organizeStopTimes takes the output of substoptimes and makes it ready for
      # comparison against the SIRI data frame

      Stimes2 <- organizeStopTimes(Stimes = StimesforSIRI, SIRIdf3 = SIRIdf3)
      pbi = pbi+1
      setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                             "% done"))
      print("created Stimes2")

      # and this part will remove duplicates
      # to check this does not need to change the DF
      # SIRIdf <- SIRIdf[!duplicated(SIRIdf$key),]

      #Only for one line... this will not work for multiple lines
      SIRIstops <- StopsForSIRI(SIRI = SIRIdf3,stops = GTFSstops) # DF of staions per line
      pbi = pbi+1
      setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                             "% done"))
      print("created SIRIstops")

      if(length(SIRIdf3$Longitude) == length(SIRIdf3$Longitude[is.na(SIRIdf3$Longitude)])){

        print(paste("failed number: ", w))
        w <- w+1

      }else{


        # for a generic version you can use SIRItoSP with use of an EPSG code, and
        spSIRI <- SIRItoILTM(SIRIdf3) # change siriDF to point with ITM
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("created spSIRI")
        if(NROW(spSIRI[!is.na(spSIRI@data$trip_id),]) > 1){
          spSIRI <- spSIRI[!is.na(spSIRI@data$trip_id),]
          # find outliers
          spSIRI <- check_outlier(spSIRI)

        }else{
          spSIRI <- spSIRI[!is.na(spSIRI@data$OriginAimedDepartureTime),]

          spSIRI@data$outlier = 3

        }
        # spSIRI2 <- SIRItoSP(SIRIdf, 2039) # change siriDF to point with selected EPSG CRS
        spstops <- stopstoILTM(SIRIstops) # change pointsDF to point with ITM
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("created spstops")
        # spstops2 <- stopstoSP(SIRIstops, 2039) # change pointsDF to point with selected EPSG CRS


        # nearest stop returns a SpatialPointsDataFrame object
        # if you want it to save to dataframe use the last row (SIRIdf2 <- spSIRI@data)
        spSIRI <- NearestStop(spSIRI,spstops)
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("Finished NearestStop")

        SIRIdf4 <- spSIRI@data
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("created SIRIdf4")

        # subsets the data frame further, leaving only the colsest call, per stop, per trip

        SIRIdf5 <- SIRIKeepClosestStop(SIRIdf4)
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("created SIRIdf5")
        # check what is the range of the times selected


        # both these actions did not require specific functions, they join the SIRI
        # data to it's rellevant GTFSstop_times data and creates a time difference column
        # which is used to check the amount of time the bus was early/late per stop.
        pbi = pbi+1
        setTkProgressBar(pb, pbi, label=paste( round(pbi/total*100, 0),
                                               "% done"))
        print("Finishing touches")
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
    close(pb)
    print(end-looptime)

    if(w >= length(linerefs)+1){
      print(paste("Finished All Bus lines in: ", end-start))}
    buses <- rbindlist(listallbuses, fill = TRUE)
    buses <- buses[!is.na(buses$timediff),]
    assign(dfname,buses, envir = .GlobalEnv)

  }


}

