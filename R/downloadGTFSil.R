#' @importFrom utils download.file
#' @importFrom easycsv choose_dir
#' @export

downloadGTFSil <- function(directory = NULL){
  if(is.null(directory)){
    directory <- easycsv::choose_dir()
  }
  else{
    directory = directory
  }
  date <- Sys.Date()
  date <- as.character(as.POSIXct(strptime(date, format = "%Y-%m-%d")))
  file_name <- as.character(paste("GTFS",date,".zip", sep = ""))
  download.file("ftp://gtfs.mot.gov.il/israel-public-transportation.zip", destfile = paste0(directory,file_name), method = "libcurl")}
