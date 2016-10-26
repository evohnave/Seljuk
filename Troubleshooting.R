suppressWarnings(library(rvest, quietly = TRUE, warn.conflicts = FALSE))
suppressWarnings(library(dplyr, quietly = TRUE, warn.conflicts = FALSE))
suppressWarnings(library(stringi, quietly = TRUE, warn.conflicts = FALSE))
suppressWarnings(library(magrittr, quietly = TRUE, warn.conflicts = FALSE))
if (!grepl(pattern = 'Seljuk$', x = getwd(),ignore.case = FALSE)) {
  print('Seljuk is not working directory.\nPlease set proper working directory.')
  stop()
}
startUp <- './SeljukStartup.R'
source(startUp)
#Set up an item for testing, initialize the DF
itm <- '232110056542'
SeljukDF <- InitializeSeljuks()

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
imgURLs <- paste(imgList, "/s-l1200.jpg", sep = "") %>% unique()
