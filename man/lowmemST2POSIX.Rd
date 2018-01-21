\name{lowmemST2POSIX}
\alias{lowmemST2POSIX}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
convert Stop_times columns to POSIXct on low memory machines
}
\description{
A possible replacement function for StopTimes2POSIXct.
Is not used by default.
used to convert Date and Time columns in the GTFS Stop_times table to a conformable POSIXct.
}
\usage{
lowmemST2POSIX(column, SIRIref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column}{
%%     ~~Describe \code{x} here~~
  A \link[base]{vector} to convert, should be a column in the GTFS Stop_Times table.
}
  \item{SIRIref}{
  a SIRI refrence table from which to get the date
  }
}
\value{
\link[base]{vector}
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
This function is not included in the STG process natively.
those who whish to use it need to redefine \link[SIRItoGTFS]{organizeStopTimes} to use it instead of StopTimes2POSIXct.
}
\seealso{
\link[SIRItoGTFS]{StopTimes2POSIXct}
\link[SIRItoGTFS]{STG}
}
\examples{
column <- lowmemST2POSIX(column, SIRIref)
}
\keyword{ ~misc }
