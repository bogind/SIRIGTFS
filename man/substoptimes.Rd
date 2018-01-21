\name{substoptimes}
\alias{substoptimes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Subset the stop_times table
}
\description{
Creates a simple subset of the GTFS to refrence the SIRI data frame
}
\usage{
substoptimes(SIRIdf,
                GTFSstop_times. = GTFSstop_times,
                GTFSroutes. = GTFSroutes,
                GTFScalendar. = GTFScalendar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SIRIdf}{
%%     ~~Describe \code{x} here~~
A SIRI data frame to use as refrence
}
  \item{GTFSstop_times.}{
  The Stop times table from the GTFS, default name is GTFSstop_times
  }
  \item{GTFSroutes.}{
  The Routes table from the GTFS, default name is GTFSroutes
  }
    \item{GTFScalendar.}{
  The Calendar table from the GTFS, default name is TFScalendar
  }
}
\details{
creates a smaller subset of the GTFS stop_times table to use in further analysis of the SIRI data.
the function is a part of STG and should not be used on it's own.
}
\value{
A \link[base]{data.frame} with the exact same columns as the GTFSstop_times table, with only the rows relevant to the SIRI data present
}
\value{
A \link[base]{data.frame} containing the final result of \link[SIRItoGTFS]{STG} with a trip number column added.
}
\references{
Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Using Big Data and open source tools for public transport reliability estimation}
}
\section{Warning}{
Do Not use this function on it's own, it is meant to be used only as part of the STG process
}
\seealso{
\link[SIRItoGTFS]{STG}
}
\keyword{ ~misc }

