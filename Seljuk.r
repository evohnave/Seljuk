GetCompletedItems <- function(){
    # Get list of latest completed auctions using search string
    #  Current search string is "LANZ Rumseldschuken"
    #  Later, perhaps change this to get other types of items from
    #   other sellers
    # LANZ Rumseldschuken search string
    searchURL <- "http://www.ebay.de/sch/Turkey/45159/i.html?_from=R40&_nkw=LANZ+Rumseldschuken&_in_kw=1&_sacat=45159&LH_Complete=1&_sadis=15&_sargn=-1%26saslc%3D1&_salic=1&_fss=1&_fsradio=%26LH_SpecificSeller%3D1&_saslop=1&_sasl=numismatiklanz&_sop=10&_dmd=7&_ipg=200"
    
    library(rvest)
    # Get the web page
    html <- read_html(searchURL, verbose = TRUE)
    # Get the <li> tags
    nodes <- html_nodes(x = html, xpath = "//li")
    # Look for <li>'s with Artikel
    zz<-grepl(pattern = "*Artikel:",x = nodes)
    nodes <- nodes[zz]
    # now artNums has twice the number of li's as items I'm interested in... 
    #  Found this at Stack Overflow...http://stackoverflow.com/ __
    #  /questions/34511885/convert-xml-nodeset-to-data-frame
    #  Converts the {xml_nodeset}
    df<- bind_rows(lapply(xml_attrs(nodes), 
                          function(x) data.frame(as.list(x), 
                                                 stringsAsFactors=FALSE)))
    artNums <- df$listingid
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
Get1CompletedItem <- function(itm){
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
    results <- getEbayImagesAndTitle(html)
    title <- results[[1]]
    imageURLs <- results[[2]]
    saveEbayImage(itm, imageURLs)
    writeToMongoDB(itm, price, endTime, title, imageURLs) # Not Done yet!
}

saveEbayImage <- function(item, imgUrls) {
    # Expect: item to be the 12 digit item number
    #         imgUrls to be 2 image urls
    # Will save to default directory, one as item + o.jpg, one as item + r.jpg
    defaultDirectory <- "D:/Downloads/Seljuk/"
    destFileName <- paste(defaultDirectory, item, c("o","r"),".jpg", sep = "")
    lapply(X = 1:2, FUN = function(x){download.file(url = imgUrls[x],
                                        destfile = destFileName[x],
                                        mode = "wb")})
}

getEbayImagesAndTitle <- function(html){
    # Assumes 2 images available
    # No error checking
    imgs <- html_nodes(html, xpath = "//img")
    # There's a faster way to do this but...
    # Get the kind of img src I want
    imgList <- imgs[grepl(pattern = "http://i.ebayimg.com/t/*",
                          x = imgs,ignore.case = TRUE)] %>% xml_attr(attr = "src")
    # Get rid of the beginning
    imgList <- gsub(pattern = "http://i.ebayimg.com/t/",
                    x = imgList, replacement = "",
                    ignore.case = TRUE)
    # Strip off the /$_##.JPG
    imgList <- gsub(pattern = "\\/\\$_[0-9]{2}\\.JPG",
                    x = imgList, replacement = "",
                    ignore.case = TRUE)
    # Split on /00/s/
    imgList <- stri_split(imgList, regex = "\\/00\\/s\\/")
    # Sort the strings
    imgList <- matrix(data = unlist(imgList), byrow = TRUE, ncol = 2)
    title <- unique(imgList[ ,1])
    imgs <- unique(imgList[ ,2])
    # Rebuild image urls
    imgs <- paste("http://i.ebayimg.com/t/", title, "/00/s/", imgs, 
                  "/$_10.jpg", sep = "")
    # now return title and imgs
    return(list(title, imgs))
}