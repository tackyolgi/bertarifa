# évek közötti összehasonlítása a random forestnek és a regressziós becslésnek

library(randomForest)
library(rpart)
library(DAAG)
library(tictoc)
library(plyr)

setwd("~/phd/research/Bertarifa4/data")

# adatbetöltés
data_11 <- read.csv("tarifa2011_b2.csv")
data_12 <- read.csv("tarifa2012_b2.csv")
data_13 <- read.csv("tarifa2013_b2.csv")
data_14 <- read.csv("tarifa2014_b2.csv")
data_15 <- read.csv("tarifa2015_b2.csv")

# factors
data_11$iskveg9 <-  as.factor(data_11$iskveg9) 
data_11$kra <- as.factor(data_11$kra) 
data_11$ag1 <- as.factor(data_11$ag1)
data_11$kol <- as.factor(data_11$kol)
data_11$ara <- as.factor(data_11$ara)
data_11$kshreg <- as.factor(data_11$kshreg)
data_11$nem <- as.factor(data_11$nem) 
data_11$ujbel <- as.factor(data_11$ujbel)
data_11$ttip <- as.factor(data_11$ttip)

data_12$iskveg9 <-  as.factor(data_12$iskveg9) 
data_12$kra <- as.factor(data_12$kra) 
data_12$ag1 <- as.factor(data_12$ag1)
data_12$kol <- as.factor(data_12$kol)
data_12$ara <- as.factor(data_12$ara)
data_12$kshreg <- as.factor(data_12$kshreg)
data_12$nem <- as.factor(data_12$nem) 
data_12$ujbel <- as.factor(data_12$ujbel)
data_12$ttip <- as.factor(data_12$ttip)

data_13$iskveg9 <-  as.factor(data_13$iskveg9) 
data_13$kra <- as.factor(data_13$kra) 
data_13$ag1 <- as.factor(data_13$ag1)
data_13$kol <- as.factor(data_13$kol)
data_13$ara <- as.factor(data_13$ara)
data_13$kshreg <- as.factor(data_13$kshreg)
data_13$nem <- as.factor(data_13$nem) 
data_13$ujbel <- as.factor(data_13$ujbel)
data_13$ttip <- as.factor(data_13$ttip)

data_14$iskveg9 <-  as.factor(data_14$iskveg9) 
data_14$kra <- as.factor(data_14$kra) 
data_14$ag1 <- as.factor(data_14$ag1)
data_14$kol <- as.factor(data_14$kol)
data_14$ara <- as.factor(data_14$ara)
data_14$kshreg <- as.factor(data_14$kshreg)
data_14$nem <- as.factor(data_14$nem) 
data_14$ujbel <- as.factor(data_14$ujbel)
data_14$ttip <- as.factor(data_14$ttip)

data_15$iskveg9 <-  as.factor(data_15$iskveg9) 
data_15$kra <- as.factor(data_15$kra) 
data_15$ag1 <- as.factor(data_15$ag1)
data_15$kol <- as.factor(data_15$kol)
data_15$ara <- as.factor(data_15$ara)
data_15$kshreg <- as.factor(data_15$kshreg)
data_15$nem <- as.factor(data_15$nem) 
data_15$ujbel <- as.factor(data_15$ujbel)
data_15$ttip <- as.factor(data_15$ttip)

# alminták
tarifred_11 <- subset(data_11, fforma==1 & atip==4)
tarifred_12 <- subset(data_12, fforma==1 & atip==4)
tarifred_13 <- subset(data_13, fforma==1 & atip==4)
tarifred_14 <- subset(data_14, fforma==1 & atip==4)
tarifred_15 <- subset(data_15, fforma==1 & atip==4)

# parameters
  # random forest
  num_tree <- 300      # ntree
  valt_szama <- 4      # mtry
  level_szam <- 10    # nodesize


# eredmények mátrix
mse_adott_evre<-matrix(0, 5, 2)
mse_adott_evre <- cbind(c(2011:2015),mse_adott_evre)
mse_adott_evre <- rbind(c("Ev","Regresszio","Regresszió négyzetekkel"), mse_adott_evre)

mse_ket_ev_kozott<-matrix(0, 4, 2)
mse_ket_ev_kozott <- cbind(c(2012:2015),mse_ket_ev_kozott)
mse_ket_ev_kozott <- rbind(c("Becsles eve","Regresszio","Regresszió négyzetekkel"), mse_ket_ev_kozott)

# becslések
# Regressziók
  # 2011
    regr_11<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
          + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_11)

    mse_adott_evre[2,2] <- mean((regr_11$fitted.values-regr_11$model$lnker)^2)

    pred_reg_12 <- predict(regr_11, tarifred_12)

    mse_ket_ev_kozott[2,2] <- mean((tarifred_12$lnker-pred_reg_12)^2, na.rm=TRUE)



regr_12<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_12)

mse_adott_evre[3,2] <- mean((regr_12$fitted.values-regr_12$model$lnker)^2)

pred_reg_13 <- predict(regr_12, tarifred_13)

mse_ket_ev_kozott[3,2] <- mean((tarifred_13$lnker-pred_reg_13)^2, na.rm=TRUE)



regr_13<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_13)

mse_adott_evre[4,2] <- mean((regr_13$fitted.values-regr_13$model$lnker)^2)

pred_reg_14 <- predict(regr_13, tarifred_14)

mse_ket_ev_kozott[4,2] <- mean((tarifred_14$lnker-pred_reg_14)^2, na.rm=TRUE)



regr_14<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_14)

mse_adott_evre[5,2] <- mean((regr_14$fitted.values-regr_14$model$lnker)^2)

pred_reg_15 <- predict(regr_14, tarifred_15)

mse_ket_ev_kozott[5,2] <- mean((tarifred_15$lnker-pred_reg_15)^2, na.rm=TRUE)

regr_15<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ ag1+ exp + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_15)

mse_adott_evre[6,2] <- mean((regr_15$fitted.values-regr_15$model$lnker)^2)

# Regressziók négyzetes tagokkal
  # 2011
    regr_11<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+kor^2 +ag1+ exp + exp^2+ kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_11)

    mse_adott_evre[2,3] <- mean((regr_11$fitted.values-regr_11$model$lnker)^2)

    pred_reg_12 <- predict(regr_11, tarifred_12)

    mse_ket_ev_kozott[2,3] <- mean((tarifred_12$lnker-pred_reg_12)^2, na.rm=TRUE)


  # 2012
    regr_12<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ kor^2+ ag1+ exp +exp^2 + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_12)

    mse_adott_evre[3,3] <- mean((regr_12$fitted.values-regr_12$model$lnker)^2)

    pred_reg_13 <- predict(regr_12, tarifred_13)

    mse_ket_ev_kozott[3,3] <- mean((tarifred_13$lnker-pred_reg_13)^2, na.rm=TRUE)


  # 2013

    regr_13<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ kor^2+ ag1+ exp+exp^2 + kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_13)

    mse_adott_evre[4,3] <- mean((regr_13$fitted.values-regr_13$model$lnker)^2)

    pred_reg_14 <- predict(regr_13, tarifred_14)

    mse_ket_ev_kozott[4,3] <- mean((tarifred_14$lnker-pred_reg_14)^2, na.rm=TRUE)


  # 2014
    regr_14<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+ kor^2+ ag1+ exp +exp^2+ kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_14)

    mse_adott_evre[5,3] <- mean((regr_14$fitted.values-regr_14$model$lnker)^2)

    pred_reg_15 <- predict(regr_14, tarifred_15)

    mse_ket_ev_kozott[5,3] <- mean((tarifred_15$lnker-pred_reg_15)^2, na.rm=TRUE)

  # 2015
    regr_15<-lm(formula=lnker~iskveg9+ letszam_bv1 + kra + kor+kor^2+ ag1+ exp +exp^2+ kol+ ara+ 
              + szolgho+ kshreg + nem+ ujbel+ ttip,data=tarifred_15)

    mse_adott_evre[6,3] <- mean((regr_15$fitted.values-regr_15$model$lnker)^2)

# random Forest
tic("rand forest 2011")
randfor_11 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                             kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg +
                             nem+ ujbel+ ttip, na.action=na.omit, mtry=valt_szama, 
                             nodesize=level_szam, ntree=num_tree, importance = T, 
                             data=tarifred_11)
toc()

mse_adott_evre[2,4] <- mean((randfor_11$y-randfor_11$predicted)^2)

pred_rf_12<-predict(randfor_11,tarifred_12)

mse_ket_ev_kozott[2,4] <- mean((tarifred_12$lnker-pred_rf_12)^2, na.rm=TRUE)



tic("rand forest 2012")
randfor_12 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                             kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg +
                             nem+ ujbel+ ttip, na.action=na.omit, mtry=valt_szama, 
                             nodesize=level_szam, ntree=num_tree, importance = T, 
                             data=tarifred_12)
toc()

mse_adott_evre[3,4] <- mean((randfor_12$y-randfor_12$predicted)^2)

pred_rf_13<-predict(randfor_12,tarifred_13)

mse_ket_ev_kozott[3,4]  <- mean((tarifred_13$lnker-pred_rf_13)^2, na.rm=TRUE)


tic("rand forest 2013")
randfor_13 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                             kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg +
                             nem+ ujbel+ ttip, na.action=na.omit, mtry=valt_szama, 
                             nodesize=level_szam, ntree=num_tree, importance = T, 
                             data=tarifred_13)
toc()

mse_adott_evre[4,4] <- mean((randfor_13$y-randfor_13$predicted)^2)

pred_rf_14<-predict(randfor_13,tarifred_14)

mse_ket_ev_kozott[4,4] <- mean((tarifred_14$lnker-pred_rf_14)^2, na.rm=TRUE)


tic("rand forest 2014")
randfor_14 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                             kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg +
                             nem+ ujbel+ ttip, na.action=na.omit, mtry=valt_szama, 
                             nodesize=level_szam, ntree=num_tree, importance = T, 
                             data=tarifred_14)
toc()

mse_adott_evre[5,4] <- mean((randfor_14$y-randfor_14$predicted)^2)

pred_rf_15<-predict(randfor_14,tarifred_15)

mse_ket_ev_kozott[5,4] <- mean((tarifred_15$lnker-pred_rf_15)^2, na.rm=TRUE)

tic("rand forest 2015")
randfor_15 <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                             kor+ ag1+ exp + kol+ ara+ szolgho+ kshreg +
                             nem+ ujbel+ ttip, na.action=na.omit, mtry=valt_szama, 
                             nodesize=level_szam, ntree=num_tree, importance = T, 
                             data=tarifred_15)
toc()

mse_adott_evre[6,4] <- mean((randfor_15$y-randfor_15$predicted)^2)

# mentés
setwd("~/phd/research/Bertarifa4/results")

write.csv2(randfor_11$importance,file="randfor_11_imp.csv")
write.csv2(randfor_12$importance,file="randfor_12_imp.csv")
write.csv2(randfor_13$importance,file="randfor_13_imp.csv")
write.csv2(randfor_14$importance,file="randfor_14_imp.csv")
write.csv2(randfor_14$importance,file="randfor_15_imp.csv")

write.csv2(randfor_11$mse,file="randfor_11_mse.csv")
write.csv2(randfor_12$mse,file="randfor_12_mse.csv")
write.csv2(randfor_13$mse,file="randfor_13_mse.csv")
write.csv2(randfor_14$mse,file="randfor_14_mse.csv")
write.csv2(randfor_14$mse,file="randfor_15_mse.csv")

