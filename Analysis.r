GraphCoins <- function(){
    source("Seljuk.r")
    
    SeljuksDF <- InitializeSeljuks()
    par(mar = c(4, 3, 3, 2) + 0.1)
    par(mfrow = c(5, 2))
    vals <- SeljuksDF$Price
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For LANZ Seljuk Offerings",
                          xlab="Price in Euro"))
    
    vals <- vals[as.numeric(vals) < 200]
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For LANZ Seljuk Offerings\nwithout outliers",
                          xlab="Price in Euro"))
    
    SeljuksDF <- InitializeSeljuks()
    PossTughril <- grepl(pattern = "*Tughril*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[PossTughril]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For Tughril",
                          xlab="Price in Euro"))
    
    PossKA2 <- grepl(pattern = "*Kilij-Arslan-II*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[PossKA2]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For II. Kiliç Arslan",
                          xlab="Price in Euro"))
    
    PossKKB <- grepl(pattern = "*Kay-Kobad*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[PossKKB]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For Keyqubad",
                          xlab="Price in Euro"))
    
    Poss3B <- grepl(pattern = "*3-Brueder*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[Poss3B]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For 3 Brothers",
                          xlab="Price in Euro"))
    
    PossKK2 <- grepl(pattern = "*Kay-Kawus-II-*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[Poss3B]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For II. Keyhüsrev",
                          xlab="Price in Euro"))
    
    PossKK3 <- grepl(pattern = "*Kay-Kawus-III-*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[Poss3B]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For III. Keyhüsrev",
                          xlab="Price in Euro"))
    
    PossM2 <- grepl(pattern = "*Masud-II-*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[Poss3B]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For II. Masud",
                          xlab="Price in Euro"))
    
    PossKK1 <- grepl(pattern = "*Kay-Kawus-I-*", x = SeljuksDF$Title)
    vals <- SeljuksDF$Price[Poss3B]
    vals <- gsub(pattern = "EUR ", replacement = "", x = vals)
    vals <- gsub(pattern = ",", replacement = ".", x = vals)
    suppressWarnings(plot(density(as.numeric(vals), from = 0),
                          main="Price Density For I. Keyhüsrev",
                          xlab="Price in Euro"))
    
}