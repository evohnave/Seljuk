SeljukStartUp <- function(){
  library(jsonlite)
  
  file_name <- list.files(paste(Sys.getenv(x = "APPDATA"),
                                "Dropbox", 
                                sep="/"), 
                          pattern = "*.json", 
                          full.names = TRUE)
  if (length(file_name)==0){
    file_name <- list.files(paste(Sys.getenv(x = "LOCALAPPDATA"),
                                  "Dropbox",
                                  sep="/"),
                            pattern = "*.json",
                            full.names = TRUE)
  }
  file_content <- fromJSON(txt = file_name)$personal
  dropBox <- file_content$path
  wd <- getwd()
  if(!grepl("/Dropbox/Seljuk", wd)){
    setwd(paste(dropBox, "\\Seljuk", sep = ""))
  }
    listOf <- dir(pattern = "*.r$")
    res <- lapply(X = listOf, FUN = source)
}