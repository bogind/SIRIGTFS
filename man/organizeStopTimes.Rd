\name{organizeStopTimes}
\alias{organizeStopTimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Organizes the gtfs Stop_times table
}
\description{
Requires the product of \link[SIRItoGTFS]{substoptimes}.
Prepares the subset of the GTFS stop_times table for comparison with SIRI's real-time data.
}
\usage{
organizeStopTimes(Stimes, SIRIdf3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Stimes}{
%%     ~~Describe \code{x} here~~
the product of \link[SIRItoGTFS]{substoptimes}, a GTFS stop_times \link[base]{data.frame} subset.
}
  \item{SIRIdf3}{
  A SIRI \link[base]{data.frame} after \link[SIRItoGTFS]{organizeSIRIdf}.
  }
}
\value{
A \link[base]{data.frame} containing columns which can be compared to SIRI's date and time columns.
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
\link[SIRItoGTFS]{substoptimes}
\link[SIRItoGTFS]{organizeSIRIdf}
}
\keyword{ ~misc }
