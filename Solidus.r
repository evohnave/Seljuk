GetSolidusSeljuks <- function(){
    
    # Gets Seljuk lots from Solidus on ebay.de
    
    csvFileName <- "Solidus.csv"
    SeljuksDF <- InitializeSolidusSeljuks()
    CompletedItems <- GetCompletedItems(SolidusDE)
    if(length(CompletedItems) == 200){
        # Might be more to get; not going to check if there are more than 400
        MoreCompletedItems <- GetCompletedItems(paste(SolidusDE,"&_pgn=2", sep = ""))
        CompletedItems[201:(200 + length(MoreCompletedItems))] <- MoreCompletedItems
    }
    CompletedItems <- RemoveDoneItems(CompletedItems, SeljuksDF$Item_Number)
    if(length(CompletedItems)>0){
        for(i in 1:(length(CompletedItems))){
            SeljuksDF <- Get1CompletedSolidusItem(CompletedItems[i], SeljuksDF)
        }
    }
    write.csv(x = SeljuksDF, file = csvFileName, row.names = FALSE)
    print(paste("Got", length(CompletedItems),"new Seljuk coins from Solidus."))
}

InitializeSolidusSeljuks <- function(){
    # Read CSV file, create DF
    return(read.csv("Solidus.csv", stringsAsFactors = FALSE,
                    colClasses = rep("character", 15)))
}

Get1CompletedSolidusItem <- function(itm, SeljuksDF){
    # Will get 1 completed item, including price, description, and pictures
    # OK, no description for now
    # Create item url
    itmURLstart <- "http://www.ebay.de/itm/"
    itmURLend <- "?nma=true&orig_cvip=true"
    itmURL <- paste(itmURLstart, itm, itmURLend, sep = "")
    # Get item
    html <- read_html(itmURL)
    price <- html_nodes(html, "#prcIsum_bidPrice") %>% xml_text()
    endTime <- html_nodes(html, "#bb_tlft") %>% xml_text() %>% stripBS()
    # Description not working, so skip for now
    imgs <- html_nodes(html, xpath = "//img")
    # There's a faster way to do this but...
    # Get the kind of img src I want
    imgList <- imgs[grepl(pattern = "http://i.ebayimg.com/t/*",
                          x = imgs,ignore.case = TRUE)] %>% xml_attr(attr = "src")
    # Get rid of the beginning
    imgList <- gsub(pattern = "http://i.ebayimg.com/t/",
                    x = imgList, replacement = "",
                    ignore.case = TRUE)
    # Special for Solidus... get rid of list items that don't start with SOL-
    imgList <- imgList[grepl("^SOL-", imgList)]
    # Strip off the /$_##.JPG
    imgList <- gsub(pattern = "\\/\\$_[0-9]{2}\\.JPG",
                    x = imgList, replacement = "",
                    ignore.case = TRUE)
    # Split on /00/s/
    imgList <- stri_split(imgList, regex = "\\/00\\/s\\/")
    # Sort the strings
    #imgList <- matrix(data = unlist(imgList[2:3]), byrow = TRUE, ncol = 2)
    imgList <- matrix(data = unlist(imgList), byrow = TRUE, ncol = 2)
    title <- unique(imgList[ ,1])
    imageURLs <- unique(imgList[ ,2])
    # Rebuild image urls
    imgs <- paste("http://i.ebayimg.com/t/", title, "/00/s/", imageURLs, 
                  "/$_10.jpg", sep = "")
    # Save the files
    destFileName <- paste("./SolidusImages/", itm,
                          "or", ".jpg", sep = "")
    download.file(url = imgs,
                  destfile = destFileName,
                  mode = "wb")
    # Save the html in the same folder as Lanz Seljuks
    destFileName <- paste("./html/", itm, ".htm", sep = "")
    download.file(url = itmURL, destfile = destFileName, mode = "wb")
    
    
    SeljuksDF <- writeToMongoDB(itm, price, endTime, title, imageURLs, SeljuksDF)
    return(SeljuksDF)
}

# Need a Solidus directory
#   Created it in this dropbox folder
#   ./dropbox/Seljuk/SolidusImages

# # Need a Solidus csv file
# # Taken from CreateCSVfiles
# # Read in sample data
# myDF <- InitializeSeljuks()[1,]
# myDF[1, 1] <- "100000000000"
# csvFileName <- "Solidus.csv"
# write.csv(x = myDF, file = csvFileName, row.names = FALSE)
# # After the whole shebang works, delete that first entry
# #   Taken from RemoveStarterEntry
# myDF <- read.csv(csvFileName, stringsAsFactors = FALSE, 
#                  colClasses = rep("character", 15))
# myDF <- myDF[-(myDF[, 1])]
# myDF <- myDF[!(myDF$Item_Number == "100000000000"), ]
# write.csv(x = myDF, file = csvFileName, row.names = FALSE)
