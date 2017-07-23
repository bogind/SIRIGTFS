\name{SIRItoPOSIXct}
\alias{SIRItoPOSIXct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
convert SIRI time column to POSIXct
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SIRItoPOSIXct(column, round)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{column}{
%%     ~~Describe \code{x} here~~
the column to reshape
}
  \item{round}{
  logical. round to nearest minute?
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
 SIRIdf$RecordedAtTime <- SIRItoPOSIXct(SIRIdf$RecordedAtTime, round = FALSE)
 SIRIdf$OriginAimedDepartureTime <- SIRItoPOSIXct(SIRIdf$OriginAimedDepartureTime)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
