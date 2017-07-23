\name{organizeStopTimes}
\alias{organizeStopTimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
organizes the Stop times table
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organizeStopTimes(Stimes, SIRIdf3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Stimes}{
%%     ~~Describe \code{x} here~~
a stop times df subseted by SIRIdf3 with substoptimes
}
  \item{SIRIdf3}{
  A SIRI df after OragniseSIRIdf
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
Stimes2 <- organizeStopTimes(Stimes = StimesforSIRI, SIRIdf3 = SIRIdf3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
