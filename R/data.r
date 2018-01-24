#' GTFS Agency table relevant to SIRI sample
#'
#' Subset of the GTFS agency table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS agency table, extracted to be used with the SIRIsample data.
#' it shows only 1 agency since the SIRIsample data only includes 1 operator in the city of Be'er Sheva.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#'
#' @format A data frame with 1 observations on the following 7 variables:
#' \describe{
#'     \item{agency_id}{An ID that uniquely identifies a transit agency}
#'     \item{agency_name}{The agency_name field contains the full name of the transit agency.}
#'     \item{agency_url}{The URL of the transit agency.}
#'     \item{agency_timezone}{The timezone where the transit agency is located. }
#'     \item{agency_lang}{A two-letter ISO 639-1 code for the primary language used by this transit agency.}
#'     \item{agency_phone}{A single voice telephone number for the specified agency, NA in this case}
#'     \item{agency_fare_url}{The URL of a web page that allows a rider to purchase tickets or other fare instruments for that agency online.}
#'     }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#agencytxt}
"GTFSagency"



#' GTFS calendar table relevant to SIRI sample
#'
#' Subset of the GTFS calendar table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS calendar table, extracted to be used with the SIRIsample data.
#' it shows the calendar data for only 1 agency since the SIRIsample data only includes 1 operator in the city of Be'er Sheva.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#' @format A data frame with 152 observations on the following 10 variables:
#' \describe{
#'      \item{service_id}{The service_id contains an ID that uniquely identifies a set of dates when service is available for one or more routes. }
#'      \item{sunday}{A binary value that indicates whether the service is valid for all Sundays.}
#'      \item{monday}{A binary value that indicates whether the service is valid for all Mondays}
#'      \item{tuesday}{A binary value that indicates whether the service is valid for all Tuesdays.}
#'      \item{wednesday}{A binary value that indicates whether the service is valid for all Wednesdays.}
#'      \item{thursday}{A binary value that indicates whether the service is valid for all Thursdays.}
#'      \item{friday}{A binary value that indicates whether the service is valid for all Fridays.}
#'      \item{saturday}{A binary value that indicates whether the service is valid for all Saturdays.}
#'      \item{start_date}{The start_date field contains the start date for the service. The start_date field's value should be in YYYYMMDD format.}
#'      \item{end_date}{The end_date field contains the end date for the service. This date is included in the service interval. The end_date field's value should be in YYYYMMDD format.}
#' }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#calendartxt}
"GTFScalendar"



#' GTFS routes table relevant to SIRI sample
#'
#' Subset of the GTFS routes table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS routes table, extracted to be used with the SIRIsample data.
#' it shows the routes data for the SIRIsample data.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#'
#' @format A data frame with 57 observations on the following 7 variables:
#' \describe{
#'     \item{route_id}{An ID that uniquely identifies a route.}
#'     \item{agency_id}{An agency for the specified route. This value is referenced from the agency file.}
#'     \item{route_short_name}{The short name of a route. This will often be a short, abstract identifier like "32", "100X", or "Green" that riders use to identify a route, set to NA due it being in UTF-8 Hebrew}
#'     \item{route_long_name}{The full name of a route, set to NA due it being in UTF-8 Hebrew}
#'     \item{route_desc}{A description of a route, set to NA due it being in UTF-8 Hebrew.}
#'     \item{route_type}{The type of transportation used on a route, in this case all values are 3 for bus.}
#'     \item{route_color}{In systems that have colors assigned to routes, the route_color field defines a color that corresponds to a route.}
#' }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#routestxt}
"GTFSroutes"



#' GTFS shapes table relevant to SIRI sample
#'
#'
#' Subset of the GTFS shapes table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS shapes table, extracted to be used with the SIRIsample data.
#' it shows the shapes data for the SIRIsample data.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#'
#' @format A data frame with 32580 observations on the following 4 variables:
#' \describe{
#'     \item{shape_id}{An ID that uniquely identifies a shape.}
#'     \item{shape_pt_lat}{A valid WGS 84 latitude. Each row in shapes represents a shape point.}
#'     \item{shape_pt_lon}{A valid WGS 84 longitude value from -180 to 180. Each row in shapes represents a shape point.}
#'     \item{shape_pt_sequence}{The point's sequence order along the shape.}
#' }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#shapestxt}
"GTFSshapes"



#' GTFS stop_times table relevant to SIRI sample
#'
#'
#' Subset of the GTFS stop_times table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS stop_times table, extracted to be used with the SIRIsample data.
#' it shows the stop times data for the SIRIsample data.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#'
#' @format A data frame with 153021 observations on the following 8 variables:
#' \describe{
#'    \item{trip_id}{An ID that identifies a trip.}
#'    \item{arrival_time}{The arrival time at a specific stop for a specific trip on a route. }
#'    \item{departure_time}{The departure time from a specific stop for a specific trip on a route.}
#'    \item{stop_id}{An ID that uniquely identifies a stop. Multiple routes may use the same stop. }
#'    \item{stop_sequence}{The order of the stops for a particular trip. }
#'    \item{pickup_type}{Indicates whether passengers are picked up at a stop as part of the normal schedule or whether a pickup at the stop is not available. 0 for Regularly scheduled pickup, 1 for No pickup available}
#'    \item{drop_off_type}{Indicates whether passengers are dropped off at a stop as part of the normal schedule or whether a drop off at the stop is not available. 0 for Regularly scheduled drop off, 1 for No drop off available.}
#'    \item{shape_dist_traveled}{The distance from the first shape point.}
#' }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#stop_timestxt}
"GTFSstop_times"



#' GTFS stops table relevant to SIRI sample
#'
#'
#' Subset of the GTFS stops table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS stop_times table, extracted to be used with the SIRIsample data.
#' it shows the stops data for the SIRIsample data, all within the city of Be'er Sheva, Israel.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#' @format A data frame with 551 observations on the following 9 variables:
#' \describe{
#'    \item{\code{stop_id}}{An ID that uniquely identifies a stop, station, or station entrance.}
#'    \item{\code{stop_code}}{A short text or a number that uniquely identifies the stop for passengers.}
#'    \item{\code{stop_name}}{The name of a stop, station, or station entrance, set to NA due it being in UTF-8 Hebrew.}
#'    \item{\code{stop_desc}}{The description of a stop, set to NA due it being in UTF-8 Hebrew.}
#'    \item{\code{stop_lat}}{The latitude of a stop, station, or station entrance. The field value must be a valid WGS 84 latitude.}
#'    \item{\code{stop_lon}}{The longitude of a stop, station, or station entrance. The field value must be a valid WGS 84 longitude value from -180 to 180.}
#'    \item{\code{location_type}}{Identifies whether this stop ID represents a stop, station, or station entrance. If no location type is specified, or the location_type is blank, stop IDs are treated as stops.}
#'    \item{\code{parent_station}}{For stops that are physically located inside stations, the field identifies the station associated with the stop.}
#'    \item{\code{zone_id}}{Defines the fare zone for a stop ID.}
#' }
#'
#' @source \url{https://developers.google.com/transit/gtfs/reference/#stopstxt}
"GTFSstops"



#' GTFS trips table relevant to SIRI sample
#'
#'
#' Subset of the GTFS trips table for Israel for the SIRIsample data.
#' This is only a small subset of the Israeli GTFS routes table, extracted to be used with the SIRIsample data.
#' it shows the trips data for the SIRIsample data.
#' note that GTFS is a global standard and not all operators fill all of the columns.
#'
#' @format A data frame with 5172 observations on the following 5 variables:
#' \describe{
#'    \item{\code{route_id}}{An ID that uniquely identifies a route.}
#'    \item{\code{service_id}}{An ID that uniquely identifies a set of dates when service is available for one or more routes. This value is referenced from the calendar table.}
#'    \item{\code{trip_id}}{An ID that identifies a trip.}
#'    \item{\code{direction_id}}{A binary value that indicates the direction of travel for a trip. 0 for travel in one direction (e.g. outbound travel), 1 for travel in the opposite direction (e.g. inbound travel).}
#'    \item{\code{shape_id}}{An ID that defines a shape for the trip. This value is referenced from the shapes table.}
#' }
#' @source \url{https://developers.google.com/transit/gtfs/reference/#tripstxt}
"GTFStrips"



#' A SIRI data frame sample
#'
#'
#' A data sample of preprocessed SIRI download,
#' includes the data for the local public transport operator in Be'er Sheva, Israel for the date of 19/07/2017
#' All columns are intentionally character vectors, this is raw data to be used in the analysis process of \code{\link{STG}}.
#' The table includes the maximum possible columns a SIRI protocol can produce( with the Israeli server, including wrapper nodes which produce NA columns),
#' The data was collected for the development of the SIRItoGTFS package and contains only one day's calls to server,
#' the "request_id" and "call_time_toServer" columns were added locally and are needed for anyone trying to use the package.
#' the data is provided for anyone wishing to test the methods used during research.
#'
#'
#' @format A data frame with 1436930 observations on the following 22 variables.
#' \describe{
#'    \item{\code{RecordedAtTime}}{Time stamp provided by the server, can differ within the same call, response rate for stops to server may vary}
#'    \item{\code{ItemIdentifier}}{The server's ID for each observation}
#'    \item{\code{MonitoringRef}}{The server's ID for the stop/group of stops}
#'    \item{\code{MonitoredVehicleJourney}}{A wrapper node, can be ignored}
#'    \item{\code{LineRef}}{The bus trip's route ID in the GTFS routes table}
#'    \item{\code{DirectionRef}}{The trip's direction code}
#'    \item{\code{PublishedLineName}}{The bus line's published name}
#'    \item{\code{OperatorRef}}{The operator's ID in the GTFS agency table}
#'    \item{\code{DestinationRef}}{The last stop's ID in the GTFS stops table}
#'    \item{\code{OriginAimedDepartureTime}}{The scheduled departure time for the trip}
#'    \item{\code{VehicleLocation}}{A wrapper node, can be ignored}
#'    \item{\code{VehicleRef}}{A unique vehicle ID}
#'    \item{\code{MonitoredCall}}{A wrapper node, can be ignored}
#'    \item{\code{Longitude}}{The observations recorded Longitude on a WGS 84 projection}
#'    \item{\code{Latitude}}{The observations recorded Latitude on a WGS 84 projection}
#'    \item{\code{StopPointRef}}{The stops' ID in the GTFS stops table}
#'    \item{\code{VehicleAtStop}}{Is the vehicle currently at the stop, by default NA which is FALSE}
#'    \item{\code{ExpectedArrivalTime}}{The predicted time the bus will arrive at the stop}
#'    \item{\code{request_id}}{A unique identifier for each call to server}
#'    \item{\code{call_time_toServer}}{Local time stamp for each call, not for each response}
#'    \item{\code{AimedArrivalTime}}{The scheduled time, only relevant for trips that have not yet begun}
#'    \item{\code{ArrivalStatus}}{In fully operational systems this should report whether the bus is early or late, in this case it does not report anything}
#' }
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
#' @aliases sirisample
"SIRIsample"



