GraphSolidusSeljuks <- function(){
    SeljuksDF <- InitializeSolidusSeljuks()
    par(mar = c(4, 3, 3, 2) + 0.1)
    # par(mfrow = c(5, 2))
    # Need to work on the legend part...
    
    Grps <- c("*",
              "Tughril-",
              "3-Br-",
              "Izz-ad-din-Kaykaus-I-",
              "Kaykhusraw-III-",
              "Masud-II-",
              "Kayyubad-I-",
              "Izz-ad-Din-Kaykaus-II-",
              "Kaykhusraw-I-",
              "Kaykhusraw-II-")
    
    plotEm <- function(x, SeljuksDF){
        who <- x
        vals <- SeljuksDF$Price[grepl(who, SeljuksDF$Title)]
        vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
        vals <- gsub(pattern = ",", replacement = ".", x = vals)
        whoFixed <- gsub("-", " ", who)
        if(whoFixed == "*") whoFixed <- "All"
        theTitle <- paste("Price Density for Solidus Seljuk Offerings, ",
                          whoFixed,
                          sep = "")
        suppressWarnings(plot(density(as.numeric(vals), from = 0),
                              main=theTitle,
                              xlab="Price in Euro"))
        
        legend(x = "topright",
           legend =  paste(length(vals), "Coins"))
    }
    
    res <- lapply(X = Grps, FUN = plotEm, SeljuksDF)
}