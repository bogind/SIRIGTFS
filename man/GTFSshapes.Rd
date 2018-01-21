\name{GTFSshapes}
\alias{GTFSshapes}
\docType{data}
\title{
GTFS shapes table relevant to SIRI sample
}
\description{
Subset of the GTFS shapes table for Israel for the SIRIsample data.
}
\usage{data("GTFSshapes")}
\format{
  A data frame with 32580 observations on the following 4 variables.
  \describe{
    \item{\code{shape_id}}{An ID that uniquely identifies a shape.}
    \item{\code{shape_pt_lat}}{A valid WGS 84 latitude. Each row in shapes represents a shape point.}
    \item{\code{shape_pt_lon}}{A valid WGS 84 longitude value from -180 to 180. Each row in shapes represents a shape point.}
    \item{\code{shape_pt_sequence}}{The point's sequence order along the shape.}
  }
}
\details{
This is only a small subset of the Israeli GTFS shapes table, extracted to be used with the SIRIsample data.
it shows the shapes data for the SIRIsample data.
note that GTFS is a global standard and not all operators fill all of the columns.
}
\source{
\url{https://developers.google.com/transit/gtfs/reference/#shapestxt}
}
\examples{
\dontrun{data(GTFSshapes)}
}
\keyword{datasets}
