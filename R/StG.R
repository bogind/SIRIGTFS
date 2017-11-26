#' @export StG


StG = function(SIRIfile,SIRIencoding,GTFSfolder,GTFSencoding,readGTFS =TRUE, dfname = "busesStG", linerefs = NULL){
    require(tcltk,quietly = TRUE)
    require(data.table,quietly = TRUE)
    require(easycsv,quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(sp, quietly = TRUE)
    require(reshape2, quietly = TRUE)
    require(rgdal, quietly = TRUE)
    require(rgeos, quietly = TRUE)

  if(exists(as.character(SIRIfile),envir = .GlobalEnv)&& is.data.frame(get(SIRIfile))){
    SIRIdf = SIRIfile
  }else{
    SIRIdf = data.table::fread(SIRIfile,encoding = SIRIencoding)
  }

  reqGTFSnames = c("GTFSstops","GTFSagency","GTFSroutes","GTFSstop_times","GTFStrips")

  if(readGTFS == FALSE && exists(as.character(reqGTFSnames),envir = .GlobalEnv)&& is.data.frame(get(reqGTFSnames)) ){
    print("GTFS in environment, proceeding.")
  }
    if(readGTFS == FALSE && !exists(as.character(reqGTFSnames),envir = .GlobalEnv)){
    stop("GTFS tables not in envir, pleas change 'readGTFS' to TRUE")
  }
    else{
    loadGTFS(GTFSfolder,encoding = GTFSencoding)
  }


  if(is.null(linerefs)){
  allroutes = GTFSroutes[GTFSroutes$route_id %in% SIRIdf$LineRef,]
  allroutes = dplyr::left_join(allroutes,GTFSagency,by = "agency_id")
  route_names = paste("Operator: ", allroutes$agency_name, " Line: ", allroutes$route_short_name, " Reference: ", allroutes$route_id)
  selected_routes = tcltk::tk_select.list(route_names, preselect = NULL, multiple = TRUE,title = "Select a Line")
  linerefs = as.integer(sapply(strsplit(selected_routes, "Reference:"), "[", 2))
  }else{
    linerefs = linerefs
  }

  w <- 1
  o <- 1
  listallbuses <- list()
  outliers <- NULL
  start <- Sys.time()


    for(lineref in linerefs){
      # SIRIdf
      looptime <- Sys.time()


      SIRIdf2 <- SubsetSIRI(SIRIdf, lineref)

      # this part will organize it and add a unique key
      # it takes some time...

      SIRIdf3 <- organizeSIRIdf(SIRIdf2, noduplicates = TRUE, round = FALSE)


      StimesforSIRI <- substoptimes(SIRIdf3, GTFSstop_times = GTFSstop_times, GTFSroutes = GTFSroutes, GTFStrips = GTFStrips,GTFScalendar = GTFScalendar)
      if(NROW(StimesforSIRI$trip_id) < 1){
        print(paste("failed number: ", w, " in subset stop times"))
        w <- w+1
      }else{

        # organizeStopTimes takes the output of substoptimes and makes it ready for
        # comparison against the SIRI data frame

        Stimes2 <- organizeStopTimes(Stimes = StimesforSIRI, SIRIdf3 = SIRIdf3)


        # and this part will remove duplicates
        # to check this does not need to change the DF
        # SIRIdf <- SIRIdf[!duplicated(SIRIdf$key),]

        #Only for one line... this will not work for multiple lines
        SIRIstops <- StopsForSIRI(SIRI = SIRIdf3,stops = GTFSstops) # DF of staions per line


        if(length(SIRIdf3$Longitude) == length(SIRIdf3$Longitude[is.na(SIRIdf3$Longitude)])){

          print(paste("failed number: ", w))
          w <- w+1

        }else{


          # for a generic version you can use SIRItoSP with use of an EPSG code, and
          spSIRI <- SIRItoILTM(SIRIdf3) # change siriDF to point with ITM
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
      assign(dfname,buses)

    }


}

