get_factors <- function(df) {
          df$nem <- as.factor(df$nem)
          df$iskveg9 <- as.factor(df$iskveg9)
          df$ujbel <- as.factor(df$ujbel)
          df$af1 <- as.factor(df$ag1)
          df$feor_2 <- as.factor(df$feor_2)
          df$ksz <- as.factor(df$ksz)
          df$kag <- as.factor(df$kag)
          df$kol <- as.factor(df$kol)
          df$ara <- as.factor(df$ara)
          df$kra <- as.factor(df$kra)
          df$kshreg <- as.factor(df$kshreg)
          df$ttip <- as.factor(df$ttip)
          
          df
}