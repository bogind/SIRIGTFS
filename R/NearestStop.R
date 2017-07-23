NearestStop <- function(SIRIspdf , Stopsspdf){
  require(sp)
  require(rgdal)
  require(dplyr)
  if(class(SIRIspdf) != "SpatialPointsDataFrame" | class(Stopsspdf) != "SpatialPointsDataFrame" ){
    print("ERROR: Both SIRIspdf and Stopsspdf must be  S4 'SpatialPointsDataFrame' type objects")
  } else {
    n <- spDists(SIRIspdf,spstops)
    n <- data.frame(n)
    colnames(n) <- spstops@data$stop_code
    nn <- data.frame(c(apply(n,1, which.min)), apply(n,1,min))
    colnames(nn) <- c("nearest:index","distance")
    s1 <- data.frame(1:(length(spstops@coords)/2),spstops@coords, spstops@data$stop_code, spstops@data$stop_id)
    colnames(s1) <- c("index","stop_lon", "stop_lat", "stop_code", "stop_id_s" )
    nn <- left_join(x = nn,y = s1, by =c ("nearest:index" = "index"))
    SIRIspdf@data <- cbind(SIRIspdf@data, nn)
    SIRIspdf
  }
}
