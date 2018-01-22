\name{StopTimes2POSIXct}
\alias{StopTimes2POSIXct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
converts a Stoptimes column to POSIXct
}
\description{
Convert a date time column from the GTFS stop_times table to POSIXct for comparison with SIRI data.
used inside of organizeStoptimes
}
\usage{
StopTimes2POSIXct(column,SIRIref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column}{
%%     ~~Describe \code{x} here~~
  the column to reshape
}
  \item{SIRIref}{
  a SIRI refrence table from which to get the date
  }
}
\details{
The function is intended to work with a SIRI reference table containing only date.
the function is a part of \link{organizeStopTimes} inside \link{STG} and should not be used on it's own.
it is also interchangable with \link{lowmemST2POSIX} for advanced user with machines low on memory.
}
\value{
a \link[base]{vector} of POSIXct values in "\%Y-\%m-\%d \%H:\%M:\%S" format
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link{STG}
\link{organizeStopTimes}
\link{lowmemST2POSIX}
}
\keyword{ ~misc }
