\name{check_outlier2}
\alias{check_outlier2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Secondary outliers test
}
\description{
Requires the product of \link[SIRItoGTFS]{SIRIKeepClosestStop}
Checks if there are sufficient observations in the data to analyse the SIRI data.
}
\usage{
check_outlier2(fullans)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fullans}{
The product of a join between the SIRI data frame after \link[SIRItoGTFS]{SIRIKeepClosestStop} and the stop times data frame after \link[SIRItoGTFS]{organizeStopTimes}
}
}
\value{
\link[base]{data.frame}
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
\link[SIRItoGTFS]{SIRIKeepClosestStop}
\link[SIRItoGTFS]{organizeStopTimes}
}
\keyword{ ~misc }
