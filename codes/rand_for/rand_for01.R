library(randomForest)

setwd("~/phd/research/Bertarifa4/data")


data <- read.csv("tarifa2015_b2.csv")

tarifred <- subset(data, fforma==1 & atip==4)
tarifred_female <- subset(tarifred, nem==0)
tarifred_male <- subset(tarifred, nem==1)
  
# factors
tarifred$iskveg9 <-  as.factor(tarifred$iskveg9) 
tarifred$kra <- as.factor(tarifred$kra) 
tarifred$ag1 <- as.factor(tarifred$ag1)
tarifred$kol <- as.factor(tarifred$kol)
tarifred$ara <- as.factor(tarifred$ara)
tarifred$kshreg <- as.factor(tarifred$kshreg)
tarifred$nem <- as.factor(tarifred$nem) 
tarifred$ujbel <- tarifred$ujbel
tarifred$ttip <- tarifred$ttip

# randomforest
tarif15s40000=tarifred[sample(nrow(tarifred), 40000), ]
(rfor2 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg + nem+ ujbel+ ttip, na.action=na.omit, mtry=5, nodesize=10, ntree=200, importance = T, data=tarif15s40000))
rfor2$importance
