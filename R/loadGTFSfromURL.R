loadGTFSfromURL <- function(urladdress){
  temp <- tempfile()
  if(is.null(urladdress)){
    download.file("ftp://gtfs.mot.gov.il/israel-public-transportation.zip", destfile = temp, method = "libcurl")
  }else{
    download.file(urladdress, destfile = temp, method = "libcurl")
  }
  fns <- unzip(zipfile = temp)
  list2env(setNames(object = lapply(fns, read.csv, encoding = 'UTF-8'),nm = make.names(paste0("GTFS",substr(gsub("*.txt$", "", fns),3, 20 )))), globalenv())
  unlink(temp)
}
