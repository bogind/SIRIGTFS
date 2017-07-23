\name{stopstoSP}
\alias{stopstoSP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
converts SIRIstops(spdf) to a TM grid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stopstoSP(GTFSstops, epsg, useSIRI, SIRI)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{GTFSstops}{
%%     ~~Describe \code{x} here~~
use a SIRIstops object
}
  \item{epsg}{
  a coordinate system code ( can be extracted using "make_EPSG" and searching the table )
  }
  \item{useSIRI}{
  logical. use a SIRIdf as refrence?
  }
  \item{SIRI}{
  if useSIRI is TRUE, which SIRIdf to use?
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
stopstoSP(SIRIstops, 4326, useSIRI = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
