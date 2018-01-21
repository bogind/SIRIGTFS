\name{SIRItoSP}
\alias{SIRItoSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Convert a SIRI data frame to SIRI spatial points data frame
}
\description{
Converts a subset of the SIRI data frame to a \link[sp]{SpatialPointsDataFrame} with the selected projection.
}
\usage{
SIRItoSP(SIRIdf, epsg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
the data frame to convert
}
  \item{epsg}{
  a coordinate reference system code (can be extracted using "make_EPSG" and searching the table )
  }
}
\details{
Requires the product of \link[SIRItoGTFS]{organizeSIRIdf},
is interchangeable with \link[SIRItoGTFS]{SIRItoILTM}.
the function is a part of STG and should not be used on it's own.
}
\value{
A \link[sp]{SpatialPointsDataFrame} with SIRI's real-time data
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
\link[SIRItoGTFS]{SIRItoILTM}
\link[SIRItoGTFS]{organizeSIRIdf}
}
\keyword{ ~misc }
