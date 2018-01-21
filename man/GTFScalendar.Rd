\name{GTFScalendar}
\alias{GTFScalendar}
\docType{data}
\title{
GTFS calendar table relevant to SIRI sample
}
\description{
Subset of the GTFS calendar table for Israel for the SIRIsample data.
}
\usage{data("GTFScalendar")}
\format{
  A data frame with 152 observations on the following 10 variables.
  \describe{
    \item{\code{service_id}}{The service_id contains an ID that uniquely identifies a set of dates when service is available for one or more routes. }
    \item{\code{sunday}}{A binary value that indicates whether the service is valid for all Sundays.}
    \item{\code{monday}}{A binary value that indicates whether the service is valid for all Mondays}
    \item{\code{tuesday}}{A binary value that indicates whether the service is valid for all Tuesdays.}
    \item{\code{wednesday}}{A binary value that indicates whether the service is valid for all Wednesdays.}
    \item{\code{thursday}}{A binary value that indicates whether the service is valid for all Thursdays.}
    \item{\code{friday}}{A binary value that indicates whether the service is valid for all Fridays.}
    \item{\code{saturday}}{A binary value that indicates whether the service is valid for all Saturdays.}
    \item{\code{start_date}}{The start_date field contains the start date for the service. The start_date field's value should be in YYYYMMDD format.}
    \item{\code{end_date}}{The end_date field contains the end date for the service. This date is included in the service interval. The end_date field's value should be in YYYYMMDD format.}
  }
}
\details{
This is only a small subset of the Israeli GTFS calendar table, extracted to be used with the SIRIsample data.
it shows the calendar data for only 1 agency since the SIRIsample data only includes 1 operator in the city of Be'er Sheva.
note that GTFS is a global standard and not all operators fill all of the columns.
}
\source{
\url{https://developers.google.com/transit/gtfs/reference/#calendartxt}
}
\examples{
\dontrun{
data(GTFScalendar)
  }
}
\keyword{datasets}
