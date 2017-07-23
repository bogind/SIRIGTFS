loadcsv_multi <- function(directory){
  if(is.null(directory)){
    directory <- choose.dir()
  }
  setwd(directory)
  temp = list.files(pattern="*.csv")
  list2env(
    lapply(setNames(temp, make.names(paste0(gsub("*.csv$", "", temp)))),
           read.csv,stringsAsFactors = FALSE, header = TRUE, quote = ""), envir = .GlobalEnv)

}
