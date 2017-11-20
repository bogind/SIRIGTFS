#' @export loadcsvfromZIP
loadcsvfromZIP <- function(filezip = file.choose(), txt = FALSE, encoding = NULL){
  if(is.null(filezip)){
    filezip <- unzip(zipfile = file.choose())
  }else{
    filezip <- unzip(zipfile = filezip)
  }
  if(txt == TRUE){
    if(is.null(encoding)){
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = ""),
                        nm = make.names(paste0("",
                                               substr(gsub("*.txt$", "", filezip),3, 20 )))), globalenv())

    }else{
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = "",
                                        encoding = encoding),
                        nm = make.names(paste0("",
                                               substr(gsub("*.txt$", "", filezip),3, 20 )))), globalenv())
    }
  }
  else{
    if(is.null(encoding)){
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = ""),
                        nm = make.names(paste0("",
                                               substr(gsub("*.csv$", "", filezip),3, 20 )))), globalenv())

    }else{
      list2env(setNames(object = lapply(filezip,
                                        read.csv,
                                        stringsAsFactors = FALSE,
                                        header = TRUE,
                                        quote = "",
                                        encoding = encoding),
                        nm = make.names(paste0("",
                                               substr(gsub("*.csv$", "", filezip),3, 20 )))), globalenv())
    }
  }


}
