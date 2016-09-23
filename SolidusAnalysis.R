SeljuksDF <- InitializeSolidusSeljuks()
par(mar = c(4, 3, 3, 2) + 0.1)
par(mfrow = c(5, 2))
vals <- SeljuksDF$Price
vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
vals <- gsub(pattern = ",", replacement = ".", x = vals)
suppressWarnings(plot(density(as.numeric(vals), from = 0),
                      main="Price Density For Solidus Seljuk Offerings",
                      xlab="Price in Euro"))