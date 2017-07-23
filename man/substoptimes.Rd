\name{substoptimes}
\alias{substoptimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Subset the stoptimes table to refrence SIRIdf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
substoptimes(SIRIdf, GTFSstop_times,GTFSroutes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
A SIRIdf to use as refrence
}
  \item{GTFSstop_times}{
  the Stop times table from the GTFS, default name is GTFSstop_times
  }
  \item{GTFSroutes}{
  the Routes table from the GTFS, default name is GTFSroutes
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
StimesforSIRI <- substoptimes(SIRIdf3, GTFSstop_times = GTFSstop_times, GTFSroutes = GTFSroutes)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
