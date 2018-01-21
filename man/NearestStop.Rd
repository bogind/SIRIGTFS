\name{NearestStop}
\alias{NearestStop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Find the nearest stop to each SIRI SpatialPoint
}
\description{
Finds the nearest stop to each SIRI SpatialPoint in the SIRI SpatialPointsDataFrame.
Requires both the SIRI SpatialPointsDataFrame and the GTFS stops SpatialPointsDataFrame.
}
\usage{
NearestStop(SIRIspdf, Stopsspdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIspdf}{
%%     ~~Describe \code{x} here~~
SIRI spatialPointsDataFrame
}
  \item{Stopsspdf}{
  GTFS Stops spatialPointsDataFrame
  }
}
\details{
Returns the SIRI \link[sp]{SpatialPointsDataFrame} with an extra column stating which GTFS stops is closest to each observation.
The function is a part of STG and should not be used on it's own.
}
\value{
\link[sp]{SpatialPointsDataFrame}
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG2 process
}
\seealso{
\link[SIRItoGTFS]{STG}
}
\keyword{ ~misc }
