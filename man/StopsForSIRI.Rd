\name{StopsForSIRI}
\alias{StopsForSIRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
subset stops for SIRIdf
}
\description{
A simple subset of the GTFS stops table to the SIRI data being used in the current analysis.
requires the SIRI data.frame to be after \link[SIRItoGTFS]{organizeSIRIdf} and the original GTFS stops table to be used.
}
\usage{
StopsForSIRI(SIRI, stops)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRI}{
%%     ~~Describe \code{x} here~~
a SIRIdf after organizeSIRIdf (SIRIdf3)
}
  \item{stops}{
  default is GTFSstops
  }
}

\value{
A subset of the GTFS stops table conforming to the bus route being evaluated.
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
}
\keyword{ ~misc }
