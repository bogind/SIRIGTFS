\name{GTFSstop_times}
\alias{GTFSstop_times}
\docType{data}
\title{
GTFS stop_times table relevant to SIRI sample
}
\description{
Subset of the GTFS stop_times table for Israel for the SIRIsample data.
}
\usage{data("GTFSstop_times")}
\format{
  A data frame with 153021 observations on the following 8 variables.
  \describe{
    \item{\code{trip_id}}{An ID that identifies a trip.}
    \item{\code{arrival_time}}{The arrival time at a specific stop for a specific trip on a route. }
    \item{\code{departure_time}}{The departure time from a specific stop for a specific trip on a route.}
    \item{\code{stop_id}}{An ID that uniquely identifies a stop. Multiple routes may use the same stop. }
    \item{\code{stop_sequence}}{The order of the stops for a particular trip. }
    \item{\code{pickup_type}}{Indicates whether passengers are picked up at a stop as part of the normal schedule or whether a pickup at the stop is not available. 0 for Regularly scheduled pickup, 1 for No pickup available}
    \item{\code{drop_off_type}}{Indicates whether passengers are dropped off at a stop as part of the normal schedule or whether a drop off at the stop is not available. 0 for Regularly scheduled drop off, 1 for No drop off available.}
    \item{\code{shape_dist_traveled}}{The distance from the first shape point.}
  }
}
\details{
This is only a small subset of the Israeli GTFS stop_times table, extracted to be used with the SIRIsample data.
it shows the stop times data for the SIRIsample data.
note that GTFS is a global standard and not all operators fill all of the columns.
}
\source{
\url{https://developers.google.com/transit/gtfs/reference/#stop_timestxt}
}
\examples{
\dontrun{data(GTFSstop_times)}
}
\keyword{datasets}
