\name{GTFSroutes}
\alias{GTFSroutes}
\docType{data}
\title{
GTFS routes table relevant to SIRI sample
}
\description{
Subset of the GTFS routes table for Israel for the SIRIsample data.
}
\usage{data("GTFSroutes")}
\format{
  A data frame with 57 observations on the following 7 variables.
  \describe{
    \item{\code{route_id}}{An ID that uniquely identifies a route.}
    \item{\code{agency_id}}{An agency for the specified route. This value is referenced from the agency file.}
    \item{\code{route_short_name}}{The short name of a route. This will often be a short, abstract identifier like "32", "100X", or "Green" that riders use to identify a route, set to NA due it being in UTF-8 hebrew}
    \item{\code{route_long_name}}{The full name of a route, set to NA due it being in UTF-8 hebrew}
    \item{\code{route_desc}}{A description of a route, set to NA due it being in UTF-8 hebrew.}
    \item{\code{route_type}}{The type of transportation used on a route, in this case all values are 3 for bus.}
    \item{\code{route_color}}{In systems that have colors assigned to routes, the route_color field defines a color that corresponds to a route.}
  }
}
\details{
This is only a small subset of the Israeli GTFS routes table, extracted to be used with the SIRIsample data.
it shows the routes data for the SIRIsample data.
note that GTFS is a global standard and not all operators fill all of the columns.
}
\source{
\url{https://developers.google.com/transit/gtfs/reference/#routestxt}
}
\examples{
\dontrun{data(GTFSroutes)}
}
\keyword{datasets}
