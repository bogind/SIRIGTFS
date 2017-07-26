loadGTFSfromZIP <- function(filezip, encoding = NULL){
  if(is.null(filezip)){
    filezip <- unzip(zipfile = file.choose())
  }else{
    filezip <- unzip(zipfile = filezip)
  }
    if(is.null(encoding)){
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = ""),
                        nm = make.names(paste0("GTFS",
                                               substr(gsub("*.txt$", "", fns),3, 20 )))), globalenv())

    }else{
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = "",
                                        encoding = encoding),
                        nm = make.names(paste0("GTFS",
                                               substr(gsub("*.txt$", "", fns),3, 20 )))), globalenv())
    }

}
