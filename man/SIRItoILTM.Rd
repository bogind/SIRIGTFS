\name{SIRItoILTM}
\alias{SIRItoILTM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
converts SIRIspdf to a TM grid
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
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
  a coordinate system code ( can be extracted using "make_EPSG" and searching the table )
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
spSIRI <- SIRItoILTM(SIRIdf3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
