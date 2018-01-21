\name{readGTFS}
\alias{readGTFS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
read GTFS files from a folder into R's environment
}
\description{
reads multiple tables into the environment, adds the "GTFS" prefix by default, based on \link[data.table]{fread} and \link[easycsv]{fread_folder}.
}
\usage{
readGTFS(directory = NULL,
                        extension = "BOTH",
                        sep="auto",
                        nrows=-1L,
                        header="auto",
                        na.strings="NA",
                        stringsAsFactors=FALSE,
                        verbose=getOption("datatable.verbose"),
                        autostart=1L,
                        skip=0L,
                        drop=NULL,
                        colClasses=NULL,
                        integer64=getOption("datatable.integer64"),
                        dec=if (sep!=".") "." else ",",
                        check.names=FALSE,
                        encoding="unknown",
                        quote="\"",
                        strip.white=TRUE,
                        fill=FALSE,
                        blank.lines.skip=FALSE,
                        key=NULL,
                        prefix="GTFS",
                        minimal=FALSE,
                        showProgress=getOption("datatable.showProgress"),
                        data.table=FALSE
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
  a directory from which to read the GTFS tables, if NULL then a manual choice is provided on windows, Linux and OSX.
  }
  \item{extension}{
  "TXT" for tables in '.txt' files, "CSV" for tables in '.csv' files, "BOTH" for both file endings. Default is "BOTH"

}
  \item{sep}{
  The separator between columns. Defaults to the first character in the set [,\verb{\t} |;:] that exists on line autostart outside quoted ("") regions, and separates the rows above autostart into a consistent number of fields, too.
  }
  \item{nrows}{
  The number of rows to read, by default -1 means all. Unlike read.table, it doesn't help speed to set this to the number of rows in the file (or an estimate), since the number of rows is automatically determined and is already fast. Only set nrows if you require the first 10 rows, for example. 'nrows=0' is a special case that just returns the column names and types; e.g., a dry run for a large file or to quickly check format consistency of a set of files before starting to read any.
  }
  \item{header}{
  Does the first data line contain column names? Defaults according to whether every non-empty field on the first data line is type character. If so, or TRUE is supplied, any empty column names are given a default name.
  }
  \item{na.strings}{
  A character vector of strings which are to be interpreted as NA values. By default ",," for columns read as type character is read as a blank string ("") and ",NA," is read as NA. Typical alternatives might be na.strings=NULL (no coercion to NA at all!) or perhaps na.strings=c("NA","N/A","null")
  }
  \item{stringsAsFactors}{
  Convert all character columns to factors?
  }
  \item{verbose}{
  Be chatty and report timings?
  }
  \item{autostart}{
  Any line number within the region of machine readable delimited text, by default 30. If the file is shorter or this line is empty (e.g. short files with trailing blank lines) then the last non empty line (with a non empty line above that) is used. This line and the lines above it are used to auto detect sep and the number of fields. It's extremely unlikely that autostart should ever need to be changed, we hope.
  }
  \item{skip}{
  If 0 (default) use the procedure described below starting on line autostart to find the first data row. skip>0 means ignore autostart and take line skip+1 as the first data row (or column names according to header="auto"|TRUE|FALSE as usual). skip="string" searches for "string" in the file (e.g. a substring of the column names row) and starts on that line (inspired by read.xls in package gdata).
  }
  \item{drop}{
  Vector of column names or numbers to drop, keep the rest.
  }
  \item{colClasses}{
  A character vector of classes (named or unnamed), as read.csv. Or a named list of vectors of column names or numbers, see examples. colClasses in fread is intended for rare overrides, not for routine use. fread will only promote a column to a higher type if colClasses requests it. It won't downgrade a column to a lower type since NAs would result. You have to coerce such columns afterwards yourself, if you really require data loss.
  }
  \item{integer64}{
  "integer64" (default) reads columns detected as containing integers larger than 2^31 as type bit64::integer64. Alternatively, "double"|"numeric" reads as base::read.csv does; i.e., possibly with loss of precision and if so silently. Or, "character".
  }
  \item{dec}{
  The decimal separator as in base::read.csv. If not "." (default) then usually ",". See details.
  }
  \item{check.names}{
  default is FALSE. If TRUE then the names of the variables in the data.table are checked to ensure that they are syntactically valid variable names. If necessary they are adjusted (by \link[base]{make.names}) so that they are, and also to ensure that there are no duplicates.
  }
  \item{encoding}{
  default is "unknown". Other possible options are "UTF-8" and "Latin-1". Note: it is not used to re-encode the input, rather enables handling of encoded strings in their native encoding.
  }
  \item{quote}{
  By default ("\""), if a field starts with a doublequote, fread handles embedded quotes robustly as explained under Details. If it fails, then another attempt is made to read the field as is, i.e., as if quotes are disabled. By setting quote="", the field is always read as if quotes are disabled.
  }
  \item{strip.white}{
  default is TRUE. Strips leading and trailing whitespaces of unquoted fields. If FALSE, only header trailing spaces are removed.
  }
  \item{fill}{
  logical (default is FALSE). If TRUE then in case the rows have unequal length, blank fields are implicitly filled.
  }
  \item{blank.lines.skip}{
  logical, default is FALSE. If TRUE blank lines in the input are ignored.
  }
  \item{key}{
  Character vector of one or more column names which is passed to setkey. It may be a single comma separated string such as key="x,y,z", or a vector of names such as key=c("x","y","z"). Only valid when argument data.table=TRUE
  }
  \item{Names}{
  A character vector of names for the tables to be read, note that the table will be read and listed by an alphabetical order, use with caution.
  }
  \item{prefix}{
  A character string to be prefixed to each table name.
  }
  \item{minimal}{
  wether or not to read all the GTFS tables or just those needed for SIRItoGTFS, default is FALSE, meaning all GTFS tables will be read
  }
  \item{showProgress}{
  TRUE displays progress on the console using \verb{\r}. It is produced in fread's C code where the very nice (but R level) txtProgressBar and tkProgressBar are not easily available.
  }
  \item{data.table}{
  logical. TRUE returns a data.table. FALSE returns a data.frame. default for SIRItoGTFS is FALSE, should be kept that way.
  }
}

\value{
Multiple \link[base]{data.frame} containing a representation of the data in the file with the "GTFS" prefix.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link[data.table]{fread}
\link[easycsv]{fread_folder}
}
\examples{
\dontrun{
require(SIRItoGTFS)
readGTFS()
#or:
readGTFS(minimal=TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~file }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~utilities }% __ONLY ONE__ keyword per line
\keyword{ ~iteration }
