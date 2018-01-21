\name{sirisample}
\alias{SIRIsample}
\docType{data}
\title{
A SIRI data frame sample
}
\description{
A data sample of preprocessed SIRI download,
includes the data for the local public transport operator in Be'er Sheva, Israel for the date of 19/07/2017
}
\usage{data("sirisample")}
\format{
  A data frame with 1436930 observations on the following 22 variables.
  \describe{
    \item{\code{RecordedAtTime}}{Time stamp provided by the server, can differ within the same call, response rate for stops to server may vary}
    \item{\code{ItemIdentifier}}{The server's ID for each observation}
    \item{\code{MonitoringRef}}{The server's ID for the stop/group of stops}
    \item{\code{MonitoredVehicleJourney}}{A wrapper node, can be ignored}
    \item{\code{LineRef}}{The bus trip's route ID in the GTFS routes table}
    \item{\code{DirectionRef}}{The trip's direction code}
    \item{\code{PublishedLineName}}{The bus line's published name}
    \item{\code{OperatorRef}}{The operator's ID in the GTFS agency table}
    \item{\code{DestinationRef}}{The last stop's ID in the GTFS stops table}
    \item{\code{OriginAimedDepartureTime}}{The scheduled departure time for the trip}
    \item{\code{VehicleLocation}}{A wrapper node, can be ignored}
    \item{\code{VehicleRef}}{A unique vehicle ID}
    \item{\code{MonitoredCall}}{A wrapper node, can be ignored}
    \item{\code{Longitude}}{The observations recorded Longitude on a WGS 84 projection}
    \item{\code{Latitude}}{The observations recorded Latitude on a WGS 84 projection}
    \item{\code{StopPointRef}}{The stops's ID in the GTFS stops table}
    \item{\code{VehicleAtStop}}{Is the vehicle currently at the stop, by defualt NA which is FALSE}
    \item{\code{ExpectedArrivalTime}}{The predicted time the bus will arrive at the stop}
    \item{\code{request_id}}{A unique identifier for each call to server}
    \item{\code{call_time_toServer}}{Local time stamp for each call, not for each response}
    \item{\code{AimedArrivalTime}}{The scheduled time, only relevant for trips that have not yet begun}
  }
}
\details{
All columns are intentionally character vectors, this is raw data to be used in the analysis process of \code{\link{STG}}.
The table includes the maximum possible columns a SIRI protocol can produce( with the Israeli server, including wrapper nodes which produce NA columns),
The data was collected for the development of the SIRItoGTFS package and contains only one day's calls to server,
the "request_id" and "call_time_toServer" columns were added locally and are needed for anyone trying to use the package.
the data is provided for anyone wishing to test the methods used during research.
}
\source{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\examples{
\dontrun{data(sirisample)}
}
\keyword{datasets}
