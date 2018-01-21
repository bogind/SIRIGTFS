\name{GTFSagency}
\alias{GTFSagency}
\docType{data}
\title{
GTFS Agency table relevant to SIRI sample
}
\description{
Subset of the GTFS agency table for Israel for the SIRIsample data.
}
\usage{data("GTFSagency")}
\format{
  A data frame with 1 observations on the following 7 variables.
  \describe{
    \item{\code{agency_id}}{An ID that uniquely identifies a transit agency}
    \item{\code{agency_name}}{The agency_name field contains the full name of the transit agency.}
    \item{\code{agency_url}}{The URL of the transit agency.}
    \item{\code{agency_timezone}}{The timezone where the transit agency is located. }
    \item{\code{agency_lang}}{A two-letter ISO 639-1 code for the primary language used by this transit agency.}
    \item{\code{agency_phone}}{A single voice telephone number for the specified agency, NA in this case}
    \item{\code{agency_fare_url}}{The URL of a web page that allows a rider to purchase tickets or other fare instruments for that agency online.}
  }
}
\details{
This is only a small subset of the Israeli GTFS agency table, extracted to be used with the SIRIsample data.
it shows only 1 agency since the SIRIsample data only includes 1 operator in the city of Be'er Sheva.
note that GTFS is a global standard and not all operators fill all of the columns.
}
\source{
\url{https://developers.google.com/transit/gtfs/reference/#agencytxt}
}
\examples{
\dontrun{
data(GTFSagency)
  }
}
\keyword{datasets}
