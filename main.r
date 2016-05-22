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
    # Clean up
    rm(list = ls())
}