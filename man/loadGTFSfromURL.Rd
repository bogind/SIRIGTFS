\name{loadGTFSfromURL}
\alias{loadGTFSfromURL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
load GTFS tables directly from a zipped file in a URL
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadGTFSfromURL(urladdress, encoding)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{urladdress}{
%%     ~~Describe \code{x} here~~
URL address of the zipped file, if NULL then Israeli GTFS will be downloaded and added to env.
}
  \item{encoding}{
  charcter. if null files will be loaded without encoding.
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
urladdress <- "ftp://gtfs.mot.gov.il/israel-public-transportation.zip"
loadGTFSfromURL(urladdress, encoding = "UTF-8")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
