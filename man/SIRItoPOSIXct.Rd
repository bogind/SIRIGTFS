\name{SIRItoPOSIXct}
\alias{SIRItoPOSIXct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
convert SIRI date time columns to POSIXct
}
\description{
For internal use in organizeSIRIdf, requires one column from a subset of the entire SIRI dataframe
}
\usage{
SIRItoPOSIXct(column, round)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column}{
%%     ~~Describe \code{x} here~~
the column to reshape
}
  \item{round}{
  logical. round to nearest minute?
  }
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used inside organizeSIRIdf only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
\link[SIRItoGTFS]{organizeSIRIdf}
}
\keyword{ ~misc }
