# évek közötti összehasonlítása a random forestnek és a regressziós becslésnek

library(randomForest)
library(DAAG)

setwd("~/phd/research/Bertarifa4/data")


data_11 <- read.csv("tarifa2011_b2.csv")
tarifred_11 <- subset(data_11, fforma==1 & atip==4)

data_12 <- read.csv("tarifa2012_b2.csv")
tarifred_12 <- subset(data_12, fforma==1 & atip==4)

data_13 <- read.csv("tarifa2013_b2.csv")
tarifred_13 <- subset(data_13, fforma==1 & atip==4)

data_14 <- read.csv("tarifa2014_b2.csv")
tarifred_14 <- subset(data_14, fforma==1 & atip==4)

data_15 <- read.csv("tarifa2015_b2.csv")
tarifred_15 <- subset(data_15, fforma==1 & atip==4)



mod<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
          +                                            szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_11)

tarifred_11_red<-tarifred_11[!is.na(tarifred_11$lnker) 
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

cvResults <- CVlm(data=tarifred_11_red,
                  form.lm=formula(lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
                  szolgho+ kshreg + nem+ ujbel+ ttip), m=100, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals.")

ss<-mean((cvResults$Predicted-cvResults$cvpred)^2/cvResults$cvpred)


# performs the CV
attr(cvResults, 'ms')


regr_11 <- lm(lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
             szolgho+ kshreg + nem+ ujbel+ ttip, tarifred_11)
pred_12 <- predict(regr_11, tarifred_12)

# abszolút eltérés
mean((tarifred_12$lnker-pred_12)^2/pred_12, na.rm=TRUE)