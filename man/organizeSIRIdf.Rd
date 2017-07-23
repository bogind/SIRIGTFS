\name{organizeSIRIdf}
\alias{organizeSIRIdf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Organize the SIRI data.frame
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
organizeSIRIdf(SIRIdf2, noduplicates = TRUE, round = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf2}{
%%     ~~Describe \code{x} here~~
A SIRI df containing only one lineref
}
  \item{noduplicates}{
  logical. Should duplicate rows be removed? default is FALSE, TRUE is recommended
  }
  \item{round}{
  logical. round all column to the nearest minute? default is FALSE, FALSE is recommended
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
Performs quite a lot of basic actions on your SIRI df,
it takes time but returns a workable data frame at the end.
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
SIRIdf3 <- organizeSIRIdf(SIRIdf2, noduplicates = TRUE, round = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
