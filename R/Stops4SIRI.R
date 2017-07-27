Stops4SIRI <- function(SIRI, stops , routes , trips , stoptimes){

  SIRIroutes <- routes[routes$agency_id %in% SIRI$OperatorRef & routes$route_short_name %in% SIRI$PublishedLineName & routes$route_type %in% SIRI$DirectionRef,]

  SIRItrips <- trips[trips$route_id %in% SIRIroutes$route_id,]

  SIRIstop_times <- stoptimes[stoptimes$trip_id %in% SIRItrips$trip_id,]

  SIRIstops <- stops[GTFSstops$stop_id %in% SIRIstop_times$stop_id,]

}
