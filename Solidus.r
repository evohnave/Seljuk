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
    imgURL <- paste(imgList, "/s-l1200.jpg", sep = "") %>% unique()
    # Save the files
    destFileName <- paste("./SolidusImages/", itm,
                          "or", ".jpg", sep = "")
    download.file(url = imgURL,
                  destfile = destFileName,
                  mode = "wb")
    # Save the html in the same folder as Lanz Seljuks
    destFileName <- paste("./html/", itm, ".htm", sep = "")
    download.file(url = itmURL, destfile = destFileName, mode = "wb")
    
    
    SeljuksDF <- writeToMongoDB(itm, price, endTime, title, imgURL, SeljuksDF)
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
