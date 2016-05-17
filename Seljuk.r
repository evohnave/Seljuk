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

# Beginning of item url
itmURLstart <- "http://www.ebay.de/itm/"

# End of item url
itmURLend <- "?nma=true&orig_cvip=true"