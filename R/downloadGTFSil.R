#' @name downloadGTFSil
#' @title download the israeli GTFS .zip file
#' @description downloads the Israeli GTFS .zip file into the selected directory with a date added to the files name.
#' @param directory If directory remains NULL the function allows you to choose a directory manually via \code{\link[easycsv]{choose_dir}}
#' @return A .zip file in the selected directory, not read into the environment
#' @note Only useful for Israeli users, other users should seek their local GTFS repository
#' @seealso \code{\link[easycsv]{choose_dir}}, \url{https://developers.google.com/transit/gtfs/}, \code{\link[utils]{download.file}}
#' @examples
#' \dontrun{
#' downloadGTFSil()
#' }
#' @keywords utilities
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
