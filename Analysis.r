source("Seljuk.r")
SeljuksDF <- InitializeSeljuks()
PossTughril <- grepl(pattern = "*Tughril*", x = SeljuksDF$Title)
vals <- SeljuksDF$Price[PossTughril]
vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
vals <- gsub(pattern = ",", replacement = ".", x = vals)
suppressWarnings(plot(density(as.numeric(vals), from = 0),
                      main="Price Density For Tughril",
                      xlab="Price in Euro"))
