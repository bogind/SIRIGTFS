\name{SubsetSIRI}
\alias{SubsetSIRI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Subset your SIRI dataset to only one lineref
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SubsetSIRI(SIRIdf, lineref)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
original SIRIdf
}
  \lineref{lineref}{
  the lineref to subset to.
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
SIRIdf2 <- SubsetSIRI(SIRIdf, 3315)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
