\name{STG}
\alias{STG}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wrapper function for the SIRItoGTFS library
}
\description{
  Performs a comparison between a SIRI data.frame and GTFS tables,
  requires the SIRI table  as well as the minimal GTFS tables to be in the environment.
  should be used after \link[SIRItoGTFS]{readGTFS}.
}
\usage{
  STG(SIRIDF, GTFSstops. = GTFSstops,
  GTFSagency. = GTFSagency,
  GTFScalendar. = GTFScalendar,
  GTFSroutes. = GTFSroutes,
  GTFSstop_times. = GTFSstop_times,
  GTFStrips. = GTFStrips,
  dfname = "busesStG",
  linerefs = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIDF}{
 A \link[base]{data.frame} containing SIRI downloaded data from a public transportation authority.
 must contain the following columns:
 "RecordedAtTime", "MonitoringRef", "LineRef", "DirectionRef",
 "PublishedLineName", "OperatorRef", "DestinationRef", "OriginAimedDepartureTime",
 "Longitude", "Latitude", "VehicleRef", "StopPointRef" & "ExpectedArrivalTime"
}
  \item{GTFSstops.}{
 A GTFS stops table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{GTFSagency.}{
A GTFS agency table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{GTFScalendar.}{
A GTFS calender table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{GTFSroutes.}{
A GTFS routes table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{GTFSstop_times.}{
A GTFS stop times table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{GTFStrips.}{
A GTFS trips table, best load into environment with \link[SIRItoGTFS]{readGTFS}
}
  \item{dfname}{
Character. The name of the output table
}
  \item{linerefs}{
Optional, a numeric vector of GTFS route_id numbers to process. if not used all route_id's in the SIRIDF provided will be used.
}
}
\details{
The function provides an "easy to use" wrapper for users unfamilliar with the functions in **SIRItoGTFS**.
It should be used after a SIRI table has been read into R's environment along with GTFS tables who have a corresponding date.
it is best used after \link[SIRItoGTFS]{readGTFS}.
the SIRI table used should have the minimal columns:
"RecordedAtTime", "MonitoringRef", "LineRef", "DirectionRef", "PublishedLineName",
"OperatorRef", "DestinationRef", "OriginAimedDepartureTime", "Longitude",
"Latitude", "VehicleRef", "StopPointRef" & "ExpectedArrivalTime".
The output table will contain a time and distance comparison between the schedule provided in the GTFS tables and the real-time data provided with the SIRI table.
}
\value{
A \link[base]{data.frame} containing a comparison between a public transportation mode's schedule and real-time data.
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\seealso{
\link[SIRItoGTFS]{readGTFS}
}
\examples{
\dontrun{
require(SIRItoGTFS)
require(data.table)
# use the sample SIRI data included with the package
SIRIDF = data("sirisample")
# load your own GTFS data
readGTFS()
# or use the subset of GTFS data conformable to the SIRI sample, also included in the package
data("GTFSstops")
data("GTFSstop_times")
data("GTFScalendar")
data("GTFStrips")
data("GTFSagency")
data("GTFSroutes")


STG(SIRIDF,
  dfname = "busesStG",
  linerefs = unique(SIRIDF$lineref[1]))
  }
}

\keyword{ ~package }
\keyword{ ~spatial }

