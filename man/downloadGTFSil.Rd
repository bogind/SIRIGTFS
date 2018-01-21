\name{downloadGTFSil}
\alias{downloadGTFSil}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
download the israeli GTFS .zip file
}
\description{
downloads the Israeli GTFS .zip file into the selected directory with a date added to the files name.
}
\usage{
downloadGTFSil(directory = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
%%     ~~Describe \code{x} here~~
If directory remains NULL the function allows you to choose a directory manually via \link[easycsv]{choose_dir}
}
}

\value{
A .zip file in the selected directory, not read into the environment
}
\note{
Only useful for Israeli users, other users should seek their local GTFS repository
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link[easycsv]{choose_dir}
\url{https://developers.google.com/transit/gtfs/}
\link[utils]{download.file}
}
\examples{
\dontrun{
downloadGTFSil()
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~utilities }% use one of  RShowDoc("KEYWORDS")
