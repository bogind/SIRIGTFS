\name{check_outlier}
\alias{check_outlier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Check for outliers in a SIRI SpatialPointsDataFrame
}
\description{
Add an outlier column to the SIRI data,
Requires the SIRI SpatialPointsDataFrame
}
\usage{
check_outlier(spSIRI)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spSIRI}{
A \link[sp]{SpatialPointsDataFrame}, containing SIRI data.
should be used after \link[SIRItoGTFS]{SIRItoILTM} or \link[SIRItoGTFS]{SIRItoSP}.
}
}
\details{
Adds an outlier column to the SIRI data,
the function is a part of STG and should not be used on it's own.
}
\value{
\link[sp]{SpatialPointsDataFrame}
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
\link[SIRItoGTFS]{SIRItoSP}
\link[sp]{SpatialPointsDataFrame}
}
\keyword{ ~misc }
