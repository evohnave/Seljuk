SeljukStartUp <- function(){
    wd <- getwd()
    if(!grepl("/Dropbox/Seljuk", wd)) setw("C:/Users/eav1/Dropbox/Seljuk")
    listOf <- dir(pattern = "*.r$")
    res <- lapply(X = listOf,FUN = source)
}