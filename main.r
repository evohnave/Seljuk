suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
suppressWarnings(library(stringi, quietly = TRUE, warn.conflicts = FALSE))

updateCoins <- function(){
    numDone <- NULL
    for(i in 1:(dim(LanzTypes)[1])){
        num <- GetGenericLANZItems(LanzTypes[i, 1])
        if(is.null(num)) num <- 0
        numDone <- rbind(numDone, list(LanzTypes[i, 1], num))
    }
    colnames(numDone) <- c("Type", "# Added")
    rownames(numDone) <- NULL
    return(numDone)
}

# Main script
GetCompletedSeljuks <- function(){
    source("Seljuk.r")
    SeljuksDF <- InitializeSeljuks()
    searchURL <- "http://www.ebay.de/sch/Turkey/45159/i.html?_from=R40&_nkw=LANZ+Rumseldschuken&_in_kw=1&_sacat=45159&LH_Complete=1&_sadis=15&_sargn=-1%26saslc%3D1&_salic=1&_fss=1&_fsradio=%26LH_SpecificSeller%3D1&_saslop=1&_sasl=numismatiklanz&_sop=10&_dmd=7&_ipg=200"
    CompletedItems <- GetCompletedItems(searchURL)
    CompletedItems <- RemoveDoneItems(CompletedItems, SeljuksDF$Item_Number)
    if(length(CompletedItems)>0){
        for(i in 1:(length(CompletedItems))){
            SeljuksDF <- Get1CompletedItem(CompletedItems[i], SeljuksDF)
        }
    }
    CloseSeljuks(SeljuksDF)
}

GetGenericLANZItems <- function(LanzType, pageNum = 0){
    # Generic version of GetCompletedSeljuks
    # LanzType here is - notice! - singular
    # Read csv file
    base <- "J:/Lanz/"
    #base <- "D:/Lanz/"    # Temp for work
    csvFileName <- paste(base, LanzType, "/", LanzType, ".csv", sep = "")
    myDF <- read.csv(csvFileName, stringsAsFactors = FALSE, 
                     colClasses = rep("character", 15))
    # Create searchURL
    searchURL <- CreateSearchURL(searchTerms = LanzTypes[(LanzTypes[, 1] == LanzType), 2], pageNum = pageNum)
    # Get completed items, remove those already done
    CompletedItems <- GetCompletedItems(searchURL)
    CompletedItems <- RemoveDoneItems(CompletedItems, myDF$Item_Number)
    # Loop over items, Get1CompletedItem
    if(length(CompletedItems)>0){
        print(paste("Adding", length(CompletedItems), "new items"))
        for(i in 1:(length(CompletedItems))){
            itm <- CompletedItems[i]
            # Create item url
            itmURLstart <- "http://www.ebay.de/itm/"
            itmURLend <- "?nma=true&orig_cvip=true"
            itmURL <- paste(itmURLstart, itm, itmURLend, sep = "")
            # Get item
            html <- read_html(itmURL)
            price <- html_nodes(html, "#prcIsum_bidPrice") %>% xml_text()
            endTime <- html_nodes(html, "#bb_tlft") %>% xml_text() %>% stripBS()
            results <- getEbayImagesAndTitle(html)
            title <- results[[1]]
            imageURLs <- results[[2]]
            if(length(imageURLs) == 1) {imageURLs <- rep(imageURLs, 2)}
            # Save images
            destFileName <- paste(base,  LanzType, "/", "Images/", itm,
                                  c("o","r"), ".jpg", sep = "")
            try(download.file(url = imageURLs[1], destfile = destFileName[1], mode = "wb"))
            try(download.file(url = imageURLs[2], destfile = destFileName[2], mode = "wb"))

            # Save html to text
            itmURLstart <- "http://www.ebay.de/itm/"
            itmURLend <- "?nma=true&orig_cvip=true"
            itmURL <- paste(itmURLstart, itm, itmURLend, sep = "")
            destFileName <- paste(base,  LanzType, "/", "html/", itm, ".htm", sep = "")
            download.file(url = itmURL, destfile = destFileName, mode = "wb")
            
            # Write item to DF
            newRow <- myDF[1, ]
            newRow[1] <- itm; newRow[2] <- title; newRow[3] <- endTime
            newRow[4] <- price; newRow[c(5:9, 12:13)] <- ""
            newRow[10] <- paste(itm, "o.jpg", sep = "")
            newRow[11] <- paste(itm, "r.jpg", sep = "")
            newRow[14:15] <- imageURLs
            myDF <- rbind(myDF, newRow)
        }
    }
    write.csv(x = myDF, file = csvFileName, row.names = FALSE)
    return(length(CompletedItems))
}

LanzTypes <- matrix(data = c("Seljuk", "Rumseldschuken",
                             "Umayyad", "Umayyaden",
                             "Khwarzimshahs", "Khwarzimshahs",
                             "Ottoman", "Osmanen+Osmanisches",
                             "Muwahhid", "Muwahhiden+Muwahhid",
                             "Ghorid", "Ghoriden",
                             "Artuqid","Urtukiden+Artukiden",
                             "Arab-Byzantine", "Arabo-Byzantiner",
                             "Abbasid", "Abbasiden",
                             "Ayyubid", "Ayyubiden",
                             "Inalid", "Inaliden",
                             "Zengid", "Zengiden",
                             "Lu'lu'id", "Luluiden",
                             "Atabegs", "Atabegs",
                             "Mameluke", "Mamelukken+Mameluken",
                             "Fatimid", "Fatimiden",
                             "Timurid", "Timuriden",
                             "Ghaznavid", "Ghaznaviden",
                             "Turkey", "Türkei+Turkei+Turkey",
                             "Mongol", "Mongolei",
                             "Sasanid", "Sasaniden",
                             "Crusaders", "Crusaders+Kreuzfahrer",
                             "Tabaristan", "Tabaristan",
                             "Armenia", "Armenien",
                             "Byzantine", "Byzantine",
                             "Achaea", "Achaea+Achaia",
                             "Venice", "Venedig",
                             "Egypt", "Egypt",
                             "Tokharistan", "Tokharistan"),
                    ncol = 2, byrow = TRUE)
colnames(LanzTypes) <- c("Type", "SearchTerm")

CreateDirectories <- function(LanzTypes) {
    base <- "J:/Lanz/"
    #base <- "D:/Lanz/"    # Temp for work
    for(i in 1:(dim(LanzTypes)[1])){
        newDir <- paste(base, LanzTypes[i, 1], "/", sep = "")
        success <- dir.create(path = newDir)
        if(success){
            htmlDir <- paste(newDir, "html/", sep = "")
            imageDir <- paste(newDir, "images/", sep = "")
            success <- dir.create(path = htmlDir)
            success <- dir.create(path = imageDir)
        }
    }
}

CreateCSVfiles <- function(LanzTypes){
    base <- "J:/Lanz/"
    #base <- "D:/Lanz/"    # Temp for work
    # Read in sample data
    myDF <- InitializeSeljuks()[1,]
    myDF[1, 1] <- "100000000000"
    for(i in 1:(dim(LanzTypes)[1])){
        csvFileName <- paste(base, LanzTypes[i, 1], "/", LanzTypes[i, 1], ".csv", sep = "")
        write.csv(x = myDF, file = csvFileName, row.names = FALSE)
    }
}

RemoveStarterEntry <- function(LanzTypes){
    base <- "J:/Lanz/"
    #base <- "D:/Lanz/"    # Temp for work
    for(i in 1:(dim(LanzTypes)[1])){
        csvFileName <- paste(base, LanzTypes[i, 1], "/", LanzTypes[i, 1], ".csv", sep = "")
        myDF <- read.csv(csvFileName, stringsAsFactors = FALSE, 
                         colClasses = rep("character", 15))
        myDF <- myDF[-(myDF[, 1])]
        myDF <- myDF[!(myDF$Item_Number == "100000000000"), ]
        write.csv(x = myDF, file = csvFileName, row.names = FALSE)
    }    
}

CreateSearchURL <- function(searchTerms, pageNum = 0){
    base <- "http://www.ebay.de/sch/Munzen/11116/i.html?-from=R40"
    keywords <- paste("&_nkw=", searchTerms, "&_in_kw=2", sep = "")
    #pagenum <- "&_pgn=2"                        # Prob not necessary

    numitems <- "&_ipg=200"
    complete <- "&LH_Complete=1"
    category <- "&_sacat=11116"
    sold <- "&LH_Sold=1"
    seller <- "&_fsradio=%26LH_SpecificSeller%3D1&_saslop=1&_sasl=numismatiklanz"
    if(pageNum != 0){
        pagenum <- paste("&_pgn=", pageNum, sep = "")
        ret <- paste(base, keywords, pagenum, numitems, complete, sold, seller, category, sep = "")
    } else {
        ret <- paste(base, keywords, numitems, complete, sold, seller, category, sep = "")
    }
    return(ret)
}
