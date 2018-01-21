\name{addtripnum}
\alias{addtripnum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Add a trip number
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Requires the final product (ans2)
adds a unique trip number
}
\usage{
addtripnum(ans2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ans}{
%%     ~~Describe \code{x} here~~
A \link[base]{data.frame}. should be a finished SIRI to GTFS table, after \link[SIRItoGTFS]{check_outlier2}
}
}
\details{
Adds a unique trip number during the day for each trip recorded in SIRI,
the function is a part of STG and should not be used on it's own.
}
\value{
A \link[base]{data.frame} containing the final result of \link[SIRItoGTFS]{STG} with a trip number column added.
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
