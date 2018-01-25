#' @name NearestStop
#' @title Find the nearest stop to each SIRI SpatialPoint
#' @description
#' Finds the nearest stop to each SIRI SpatialPoint in the SIRI SpatialPointsDataFrame.
#' Requires both the SIRI SpatialPointsDataFrame and the GTFS stops SpatialPointsDataFrame.
#' @param SIRIspdf The SIRI spatialPointsDataFrame
#' @param Stopsspdf GTFS Stops spatialPointsDataFrame
#' @details
#' Returns the SIRI \code{\link[sp]{SpatialPointsDataFrame}} with an extra column stating which GTFS stops is closest to each observation.
#' The function is a part of STG and should not be used on it's own.
#' @return
#' Adds a column containing the Nearest stop to each observation in the SIRI \code{\link[sp]{SpatialPointsDataFrame}}
#' @references Bogin, D., Levy, N. and Ben-Elia E. (2018) \emph{Estimation of public transport service reliability using Big Data and open source tools}
#' @section Warning:
#' Do Not use this function on it's own, it is meant to be used only as part of the STG process
#' @seealso \code{\link[SIRItoGTFS]{STG}}
#' @keywords misc internal
#' @importFrom  sp spDists SpatialPointsDataFrame
#' @importClassesFrom sp SpatialPointsDataFrame
#' @importFrom dplyr left_join
#' @export

NearestStop <- function(SIRIspdf , Stopsspdf){

  if(class(SIRIspdf) != "SpatialPointsDataFrame" | class(Stopsspdf) != "SpatialPointsDataFrame" ){
    print("ERROR: Both SIRIspdf and Stopsspdf must be  S4 'SpatialPointsDataFrame' type objects")
  } else {
    n <- sp::spDists(SIRIspdf,Stopsspdf)
    n <- data.frame(n)
    colnames(n) <- Stopsspdf@data$stop_code
    nn <- data.frame(c(apply(n,1, which.min)), apply(n,1,min))
    colnames(nn) <- c("nearest:index","distance")
    s1 <- data.frame(1:(length(Stopsspdf@coords)/2),Stopsspdf@coords, Stopsspdf@data$stop_code, Stopsspdf@data$stop_id)
    colnames(s1) <- c("index","stop_lon", "stop_lat", "stop_code", "stop_id_s" )
    nn <- dplyr::left_join(x = nn,y = s1, by =c ("nearest:index" = "index"))
    SIRIspdf@data <- cbind(SIRIspdf@data, nn)
    return(SIRIspdf)
  }
}
