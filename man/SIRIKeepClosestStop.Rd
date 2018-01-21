\name{SIRIKeepClosestStop}
\alias{SIRIKeepClosestStop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Keep only the closest stop to each SIRI observation
}
\description{
Removes duplicates from the SIRI data frame, and keep only the closest stop to each SIRI observation.
requires the data.frame from the product of \link[SIRItoGTFS]{NearestStop}.
}
\usage{
SIRIKeepClosestStop(SIRIdf)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
A SIRI df with a distance field (SIRIdf4)
}
}
\value{
A SIRI \link[base]{data.frame} with removed duplicates.
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
