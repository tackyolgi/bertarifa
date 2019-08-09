library(randomForest)
library(tictoc)

setwd("~/phd/research/Bertarifa4/data")

data_11 <- read.csv("tarifa2011_b2.csv")
tarifred_11 <- subset(data_11, fforma==1 & atip==4)

tarifred_11_red <- tarifred_11[!is.na(tarifred_11$lnker) 
                             & !is.na(tarifred_11$iskveg9)
                             & !is.na(tarifred_11$letszam_bv1) 
                             & !is.na(tarifred_11$kra)
                             & !is.na(tarifred_11$kor)
                             & !is.na(tarifred_11$ag1)
                             & !is.na(tarifred_11$exp)
                             & !is.na(tarifred_11$kol)
                             & !is.na(tarifred_11$ara)
                             & !is.na(tarifred_11$szolgho)
                             & !is.na(tarifred_11$kshreg)
                             & !is.na(tarifred_11$nem)
                             & !is.na(tarifred_11$ujbel)
                             & !is.na(tarifred_11$ttip),]

minta <- tarifred_11_red[sample(nrow(tarifred_11_red), 40000),]

tic()
tuneRF( cbind(minta$iskveg9, minta$letszam_bv1,
              minta$kra, minta$kor,
              minta$ag1, minta$exp, 
              minta$kol, minta$ara,
              minta$szolgho, minta$kshreg,
              minta$nem, minta$ujbel,
              minta$ttip),minta$lnker, 
              ntreeTry = 100, 
              stepFactor = 2, 
              improve = 0.05, 
              trace=TRUE, plot=TRUE)
toc()
# vannak hiányzó értékek
+ tarifred_11$kor+ tarifred_11$ag1+ tarifred_11$exp + 
  tarifred_11$kol+ tarifred_11$ara+ tarifred_11$szolgho+
  tarifred_11$kshreg + tarifred_11$nem+ tarifred_11$ujbel+ tarifred_11$ttip