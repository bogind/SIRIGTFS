\name{SubsetSIRI}
\alias{SubsetSIRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Subset your SIRI dataset to only one Route
}
\description{
A simple subset of the full SIRI dataframe to one selected bus route(lineref)
}
\usage{
SubsetSIRI(SIRIdf, lineref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
original SIRI data frame
}
  \item{lineref}{
  the lineref/route_id to be used.
  }
}
\value{
A smaller \link[base]{data.frame} of SIRI data only containing the selected lineref's observations.
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
