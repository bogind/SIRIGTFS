\name{is.outlier}
\alias{is.outlier}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
identify spatial outliers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
used to find trip that did not leave the station
}
\usage{
is.outlier(spSIRI, trip, outliers = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spSIRI}{
%%     ~~Describe \code{x} here~~
  A SIRI SpatialPointsDataFrame
}
  \item{trip}{
  A unique trip id to check
  }
  \item{outliers}{
  A vector to write outliers into. if not defined will be created with the same name.
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

if(NROW(spSIRI[!is.na(spSIRI@data$trip_id),]) > 1){

  spSIRI <- spSIRI[!is.na(spSIRI@data$trip_id),]


  for(trip in unique(spSIRI@data$trip_id)){

    is.outlier(spSIRI, trip, outliers = NULL)


  }

}else{
  spSIRI <- spSIRI[!is.na(spSIRI@data$OriginAimedDepartureTime),]

  is.outlier(spSIRI, trip, outliers = NULL)


  }


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
