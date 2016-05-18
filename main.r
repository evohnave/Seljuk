# Main script
main <- function(){
    source("Seljuk.r")
    SeljuksDF <- InitializeSeljuks()
    CompletedItems <- GetCompletedItems()
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