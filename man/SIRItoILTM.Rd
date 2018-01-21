\name{SIRItoILTM}
\alias{SIRItoILTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Converts a SIRI data frame to Israel's TM grid
}
\description{
Converts a subset of the SIRI data frame to a \link[sp]{SpatialPointsDataFrame} With Israel's TM projection (epsg 2039).
}
\usage{
SIRItoILTM(SIRIdf, epsg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
A SIRI df after organizeSIRIdf (SIRIdf3)
}
  \item{epsg}{
  a coordinate system code ( can be extracted using "make_EPSG" and searching the table ),
  defualt is NULL with Israel's TM Grid (epsg 2039)
  }
}
\details{
Requires the product of \link[SIRItoGTFS]{organizeSIRIdf},
is interchangeable with \link[SIRItoGTFS]{SIRItoSP}.
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
\link[SIRItoGTFS]{SIRItoSP}
\link[SIRItoGTFS]{organizeSIRIdf}
}
\keyword{ ~misc }
\keyword{ ~spatial }
