loadGTFS <- function(directory = NULL, encoding = NULL){
  if(is.null(directory)){
    directory <- choose.dir()
  }
  origwd <- getwd()
  setwd(directory)
  temp = list.files(pattern="*.txt")
  if(is.null(encoding)){
    list2env(
      lapply(setNames(temp,
                      make.names(paste0("GTFS",gsub("*.txt$", "", temp)))),
             read.csv,
             stringsAsFactors = FALSE,
             header = TRUE,
             quote = ""),
      envir = .GlobalEnv)
  }else{
    list2env(
      lapply(setNames(temp,
                      make.names(paste0("GTFS",gsub("*.txt$", "", temp)))),
             read.csv,
             stringsAsFactors = FALSE,
             encoding = encoding,
             header = TRUE,
             quote = ""),
      envir = .GlobalEnv)
  }
  setwd(origwd)
}
