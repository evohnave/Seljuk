WheresMyDropbox <- function(){
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
  return(file_content$path)
}