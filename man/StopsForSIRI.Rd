\name{StopsForSIRI}
\alias{StopsForSIRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
subset stops for SIRIdf
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
StopsForSIRI(SIRI, stops)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRI}{
%%     ~~Describe \code{x} here~~
a SIRIdf after organizeSIRIdf (SIRIdf3)
}
  \item{stops}{
  default is GTFSstops
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
SIRIstops <- StopsForSIRI(SIRI = SIRIdf3,stops = GTFSstops)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
