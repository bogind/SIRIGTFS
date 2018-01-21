\name{stopstoSP}
\alias{stopstoSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Converts the GTFS stops table to a SpatialPointsDataFrame
}
\description{
Converts a subset of the GTFS stops table to a \link[sp]{SpatialPointsDataFrame} with the selected projection.
}
\usage{
stopstoSP(GTFSstops, epsg, useSIRI, SIRI)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GTFSstops}{
%%     ~~Describe \code{x} here~~
A subset of the GTFS stops table, should be the product of \link[SIRItoGTFS]{StopsForSIRI}
}
  \item{epsg}{
  a coordinate system code ( can be extracted using "make_EPSG" and searching the table )
  }
  \item{useSIRI}{
  logical. use a SIRI data frame as refrence?
  }
  \item{SIRI}{
  if useSIRI is TRUE, which SIRI data frame to use?
  }
}
\details{
Requires the product of \link[SIRItoGTFS]{StopsForSIRI},
is interchangeable with \link[SIRItoGTFS]{stopstoILTM} for Israel's TM grid.
the function is a part of STG and should not be used on it's own.
}
\value{
A \link[sp]{SpatialPointsDataFrame} with stops relevant to SIRI's real-time data
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
\link[SIRItoGTFS]{stopstoILTM}
\link[SIRItoGTFS]{StopsForSIRI}
}
\examples{
\dontrun{
spstops <- stopstoSP(SIRIstops, 4326, useSIRI = FALSE)
}
}
\keyword{ ~misc }
\keyword{ ~spatial }
