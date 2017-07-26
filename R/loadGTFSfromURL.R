loadGTFSfromURL <- function(urladdress, encoding = NULL){
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
    list2env(setNames(object = lapply(fns,
                                      read.csv),
                      nm = make.names(paste0("GTFS",substr(gsub("*.txt$", "", fns),3, 20 ))))
             , globalenv())
  }else{
    list2env(setNames(object = lapply(fns,
                                      read.csv,
                                      encoding = encoding),
                      nm = make.names(paste0("GTFS",substr(gsub("*.txt$", "", fns),3, 20 ))))
             , globalenv())
  }


  unlink(temp)
}
