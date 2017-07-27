\name{loadcsv_multi}
\alias{loadcsv_multi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
load multiple csv files to the environment
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loadcsv_multi(directory, txt = FALSE, encoding = NULL)

loadcsvfromZIP(filezip, txt = FALSE, encoding = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
%%     ~~Describe \code{x} here~~
a directory to load the files from, if NULL then a manual choice is provided
}
  \item{filezip}{
  a '.zip' file to load the files from, if NULL then a manual choice is provided
  }
  \item{txt}{
  logical. if TRUE .txt files will be loaded as tables instead of .csv
  }
  \item{encoding}{
  character. files encoding. can be NULL
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
loadcsv_multi is used for uncompressed files in a single folder.
loadcsvfromZIP is used for comma seperated tables inside of a .zip file.
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
directory = "D:/Data/tables"
loadcsv_multi(directory)
#or:
loadcsv_multi()
#or if your tables are in .txt files with comma seperation:
lodacsv_multi(directory,txt = TRUE)

for tables compressed in a single .zip file:

filezip = "D:/Data/tables.zip"
loadcsvfromZIP(filezip)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
