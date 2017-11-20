#' @export loadcsv_multi
loadcsv_multi <- function(directory = choose.dir(), txt = FALSE, encoding = NULL){
  if(is.null(directory)){
    directory <- choose.dir()
  }
  placeholder = getwd()
  setwd(directory)
  if(txt == TRUE){
    temp = list.files(pattern="*.txt")
    if(is.null(encoding)){
      list2env(
        lapply(setNames(temp,
                        make.names(paste0(gsub("*.txt$", "", temp)))),
               read.csv,
               stringsAsFactors = FALSE,
               header = TRUE, quote = ""),
        envir = .GlobalEnv)
      setwd(placeholder)
    }
    else{
      list2env(
        lapply(setNames(temp,
                        make.names(paste0(gsub("*.txt$", "", temp)))),
               read.csv,
               stringsAsFactors = FALSE,
               encoding = encoding,
               header = TRUE, quote = ""),
        envir = .GlobalEnv)
      setwd(placeholder)
    }
  }
  else{
    temp = list.files(pattern="*.csv")

  if(is.null(encoding)){
    list2env(
      lapply(setNames(temp,
                      make.names(paste0(gsub("*.csv$", "", temp)))),
             read.csv,
             stringsAsFactors = FALSE,
             header = TRUE, quote = ""),
      envir = .GlobalEnv)
    setwd(placeholder)
  }
    else{
      list2env(
        lapply(setNames(temp,
                        make.names(paste0(gsub("*.csv$", "", temp)))),
               read.csv,
               stringsAsFactors = FALSE,
               encoding = encoding,
               header = TRUE, quote = ""),
        envir = .GlobalEnv)
      setwd(placeholder)
    }
 }
}
