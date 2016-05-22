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

GetGenericLANZItems <- function(LanzType) {
    # Generic version of GetCompletedSeljuks
    
}

LanzTypes <- matrix(data = c("Seljuk", "Rumseldschuken",
                             "Umayyid", "Umayyaden",
                             "Khwarizmshah", "Khwarizmshahs",
                             "Ottoman", "Osmanen%2C+Osmanisches",
                             "Muwahhid", "Muwahhiden%2C+Muwahhid",
                             "Ghorid", "Ghoriden",
                             "Artuqid","Urtukiden%2C+Artukiden",
                             "Arab-Byzantine", "Arabo-Byzantiner",
                             "Abbasid", "Abbasiden",
                             "Ayyubid", "Ayyubiden",
                             "Inalid", "Inaliden",
                             "Zengid", "Zengiden",
                             "Lu'lu'id", "Luluiden",
                             "Atabegs", "Atabegs",
                             "Mameluke", "Mamelukken%2C+Mameluken",
                             "Fatimid", "Farimiden",
                             "Timurid", "Timuriden",
                             "Ghaznavid", "Ghaznaviden",
                             "Turkey", "Türkei%2C+Turkei%2C+Turkey",
                             "Mongol", "Mongolei",
                             "Sasanid", "Sasaniden",
                             "Crusaders", "Crusaders%2C+Kreuzfahrer",
                             "Tabaristan", "Tabaristan",
                             "Armenia", "Armenien",
                             "Byzantine", "Byzantine",
                             "Achaea", "Achaea%2C+Achaia"),
                    ncol = 2, byrow = TRUE)
colnames(LanzTypes) <- c("Type", "SearchTerm")

CreateDirectories <- function(LanzTypes) {
    base <- "J:/Lanz/"
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
    # Read in sample data
    myDF <- InitializeSeljuks()[1,]
    myDF[1, 1] <- "100000000000"
    for(i in 1:(dim(LanzTypes)[1])){
        csvFileName <- paste(base, LanzTypes[i, 1], "/", LanzTypes[i, 1], ".csv", sep = "")
        write.csv(x = myDF, file = csvFileName, row.names = FALSE)
    }
}