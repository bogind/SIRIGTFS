loadGTFSfromURL <- function(urladdress = NULL, encoding = NULL){
  temp <- tempfile()
  if(is.null(urladdress)){
    download.file("ftp://gtfs.mot.gov.il/israel-public-transportation.zip",
                  destfile = temp,
                  method = "libcurl")
  }else{
    download.file(urladdress,
                  destfile = temp,
                  method = "libcurl")
  }
  tempzip <- unzip(zipfile = temp)
  if(is.null(encoding)){
    list2env(setNames(object = lapply(tempzip,
                                      read.csv),
                      nm = make.names(paste0("GTFS",substr(gsub("*.txt$", "", tempzip),3, 20 ))))
             , globalenv())
  }else{
    list2env(setNames(object = lapply(tempzip,
                                      read.csv,
                                      encoding = encoding),
                      nm = make.names(paste0("GTFS",substr(gsub("*.txt$", "", tempzip),3, 20 ))))
             , globalenv())
  }


  unlink(temp)
}
