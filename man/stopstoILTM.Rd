\name{stopstoILTM}
\alias{stopstoILTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Converts the GTFS stops table to Israel's TM grid
}
\description{
Converts a subset of the GTFS stops table to a \link[sp]{SpatialPointsDataFrame} With Israel's TM projection (epsg 2039).
}
\usage{
stopstoILTM(GTFSstops., useSIRI = FALSE, SIRI = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GTFSstops.}{
%%     ~~Describe \code{x} here~~
A subset of the GTFS stops table, should be the product of \link[SIRItoGTFS]{StopsForSIRI}
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
is interchangeable with \link[SIRItoGTFS]{stopstoSP}.
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
\link[SIRItoGTFS]{stopstoSP}
\link[SIRItoGTFS]{StopsForSIRI}
}
\keyword{ ~misc }
\keyword{ ~spatial }
