GetCompletedItems <- function(searchURL){
    # Get list of latest completed auctions using search string
    suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
    suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
    # Get the web page
    html <- read_html(searchURL, verbose = TRUE)
    # Get the <li> tags
    nodes <- html_nodes(x = html, xpath = "//li")
    df<- bind_rows(lapply(xml_attrs(nodes), 
                          function(x) data.frame(as.list(x), 
                                                 stringsAsFactors=FALSE)))
    artNums <- NULL
    if(!is.null(df$listingid)){
        artNums <- df$listingid[!(is.na(df$listingid))]}
    # OK, I've got the list of items to look at.
    return(artNums)
}

stripBS <- function(str) {
    # cleans up time for ebay.de
    # also good for other things on an ebay page
    str <- gsub(pattern = "\\t", x = str, replacement = "")
    str <- gsub(pattern = "\\n", x = str, replacement = "")
    str <- gsub(pattern = "\\r", x = str, replacement = " ")
    library(stringi)
    return(stri_trim_both(str))
}

Get1CompletedItem <- function(itm, SeljuksDF){
    # Will get 1 completed item, including price, description, and pictures
    # OK, no description for now
    # Create item url
    # Create the search URL
    itmURLstart <- "http://www.ebay.de/itm/"
    itmURLend <- "?nma=true&orig_cvip=true"
    itmURL <- paste(itmURLstart, itm, itmURLend, sep = "")
    
    # Create html
    html <- read_html(itmURL)
    price <- html_nodes(html, "#prcIsum_bidPrice") %>% xml_text()
    endTime <- html_nodes(html, "#bb_tlft") %>% xml_text() %>% stripBS()
    title <- html_node(html, "title") %>% 
      gsub(pattern = "<title>", replacement = "", ignore.case = TRUE) %>% 
      gsub(pattern = "\\W|\\S*</title>", replacement = "", ignore.case = TRUE)
    
    # Looking for images now
    imgs <- html_nodes(html, xpath = "//img")
    oldPattern <- "http://i.ebayimg.com/t/"
    newPattern <- "http://i.ebayimg.com/images/g/"
    
    if(sum(grepl(pattern = paste(newPattern, "*", sep = ""),
                 x = imgs,ignore.case = TRUE)) > 0){
      imgList <- imgs[grepl(pattern = paste(newPattern, "*", sep = ""),
                            x = imgs,ignore.case = TRUE)] %>% xml_attr(attr = "src")
    } else {
      imgList <- imgs[grepl(pattern = paste(oldPattern, "*", sep = ""),
                            x = imgs,ignore.case = TRUE)] %>% xml_attr(attr = "src")
    }
    # OK, image list is good now except for the ending... strip that off
    endPattern <- '\\/s-\\S*jpg$'
    imgList <- gsub(pattern = endPattern, 
                    replacement = '', 
                    x = imgList,
                    ignore.case = TRUE)
    # Create new image urls
    imageURLs <- paste(imgList, "/s-l1200.jpg", sep = "") %>% unique()
    saveEbayImage(itm, imageURLs)
    SaveHTMLtoText(itm)
    SeljuksDF <- writeToMongoDB(itm, price, endTime, title, imageURLs, SeljuksDF)
    return(SeljuksDF)
}

saveEbayImage2 <- function(item, imgUrls) {
    # Expect: item to be the 12 digit item number
    #         imgUrls to be 2 image urls
    # Will save to default directory, one as item + o.jpg, one as item + r.jpg
    #defaultDirectory <- "D:/Downloads/Seljuk/"
    defaultDirectory <- IdentifyFileLibrary()
    destFileName <- paste(defaultDirectory, "Images/", item,
                          c("o","r"),".jpg", sep = "")
    lapply(X = 1:2, FUN = function(x){download.file(url = imgUrls[x],
                                        destfile = destFileName[x],
                                        mode = "wb")})
}

saveEbayImage <- function(item, imgUrls) {
    # Expect: item to be the 12 digit item number
    #         imgUrls to be 2 image urls
    # Will save to default directory, one as item + o.jpg, one as item + r.jpg
    destFileName <- paste("./Images/", item,
                          c("o","r"), ".jpg", sep = "")
    lapply(X = 1:2, FUN = function(x){download.file(url = imgUrls[x],
                                                    destfile = destFileName[x],
                                                    mode = "wb")})
}

getEbayImagesAndTitle <- function(html){
    # 20161025 No title available
    # Assumes 2 images available
    # No error checking
    imgs <- html_nodes(html, xpath = "//img")
    # Old pattern
    Pattern = "http://i.ebayimg.com/t/"
    # New pattern
    newPattern = "http://i.ebayimg.com/images/g/"
    # There's a faster way to do this but...
    # Get the kind of img src I want
    imgList <- imgs[grepl(pattern = paste(newPattern, "*", sep = ""),
                          x = imgs,ignore.case = TRUE)] %>% xml_attr(attr = "src")
    # Get rid of the beginning
    imgList <- gsub(pattern = paste(newPattern, "*", sep = ""),
                    x = imgList, replacement = "",
                    ignore.case = TRUE)
    # Old
    # Strip off the /$_##.JPG
    # imgList <- gsub(pattern = "\\/\\$_[0-9]{2}\\.JPG",
    #                 x = imgList, replacement = "",
    #                 ignore.case = TRUE)
    imgList <- gsub(pattern = "\\/s-\\S*.jpg", x = imgList, replacement = "",
                    ignore.case = TRUE)
    # 20161025 Comment out the following lines since not necessary TODAY
    # # Split on /00/s/
    # imgList <- stri_split(imgList, regex = "\\/00\\/s\\/")
    # # Sort the strings
    # imgList <- matrix(data = unlist(imgList), byrow = TRUE, ncol = 2)
    # title <- unique(imgList[ ,1])
    # imgs <- unique(imgList[ ,2])
    # Get rid of extraneous starting characters
    #imgs <- gsub(pattern = "^\\S*\\/z\\/",replacement = "",x = imgs,ignore.case = TRUE)
    imgs <- unique(imgList)
    # Rebuild image urls
    imgs <- paste(newPattern, imgs, "/s-l1200.jpg", sep = "")
    # now return title and imgs
    # 20161025 not returning title
    #return(list(title, imgs))
    return(imgs)
}

IdentifyFileLibrary <- function(){
    df <- matrix(data = c(
        "SEIP06-W7", "C:/Users/eav1/Dropbox/Seljuk/",
        "DS", "C:/Users/Eric/Dropbox/Seljuk/",
        "DS", "C:/Users/Eric/Dropbox/Seljuk/",
        "DT", "C:/Users/Cire/Dropbox/Seljuk/",
        "DT2", "C:/Users/Cire/Dropbox/Seljuk/",
        "SP3", "C:/Users/Eric/Dropbox/Seljuk/"), ncol = 2, byrow = TRUE)
    return(df[df[,1]==Sys.info()["nodename"], 2])
}

writeToMongoDB <- function(itm, price, endTime, title, imageURLs, SeljuksDF){
    # First version won't use Mongo but just a csv
    # Returns new SeljuksDF
    newRow <- SeljuksDF[1, ]
    newRow[1] <- itm; newRow[2] <- title; newRow[3] <- endTime
    newRow[4] <- price; newRow[c(5:9, 12:13)] <- ""
    newRow[10] <- paste(itm, "o.jpg", sep = "")
    newRow[11] <- paste(itm, "r.jpg", sep = "")
    newRow[14:15] <- imageURLs
    return(rbind(SeljuksDF, newRow))
}

InitializeSeljuks <- function(){
    # Read CSV file, create DF
    return(read.csv("Seljuk.csv", stringsAsFactors = FALSE,
                    colClasses = rep("character", 15)))
}

CloseSeljuks <- function(SeljuksDF){
    # Writes the CSV file
    # Won't need once we go to MongoDB
    write.csv(x = SeljuksDF, file = "Seljuk.csv", row.names = FALSE)
}

SaveHTMLtoText2 <- function(item){
    # Expect: item to be the 12 digit item number
    # Will save to default directory plus 'html/'
    defaultDirectory <- IdentifyFileLibrary()
    itmURLstart <- "http://www.ebay.de/itm/"
    itmURLend <- "?nma=true&orig_cvip=true"
    itmURL <- paste(itmURLstart, item, itmURLend, sep = "")
    destFileName <- paste(defaultDirectory, "html/", item, ".htm", sep = "")
    download.file(url = itmURL, destfile = destFileName, mode = "wb")
}

SaveHTMLtoText <- function(item){
    # Expect: item to be the 12 digit item number
    # Will save to default directory plus '/html/'
    
    itmURLstart <- "http://www.ebay.de/itm/"
    itmURLend <- "?nma=true&orig_cvip=true"
    itmURL <- paste(itmURLstart, item, itmURLend, sep = "")
    destFileName <- paste("./html/", item, ".htm", sep = "")
    download.file(url = itmURL, destfile = destFileName, mode = "wb")
}

RemoveDoneItems <- function(ToDoItems, CompletedItems){
    # Removes already gotten items from ToDoItems and returns them.
    return(setdiff(ToDoItems, CompletedItems))
}