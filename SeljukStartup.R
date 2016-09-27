SeljukStartUp <- function(){
    wd <- getwd()
    if(!grepl("/Dropbox/Seljuk", wd)) setwd("C:/Users/eav1/Dropbox/Seljuk")
    listOf <- dir(pattern = "*.r$")
    res <- lapply(X = listOf,FUN = source)
}