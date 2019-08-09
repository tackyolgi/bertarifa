setwd("~/phd/research/Bertarifa4/codes")

source("data_loading.R")

library(tictoc)
library(randomForest) #tuneRF-hez kell
library(DAAG)
library(plyr)

# tarifred_11$kulcs <- as.factor(tarifred_11$kulcs)



# nõk aránya változó
table_nem<-table(tarifred_11$feor2008_4, tarifred_11$nem)
feor_nem_megoszl <- as.data.frame(prop.table(table_nem,1))
colnames(feor_nem_megoszl)<-c("feor2008_4","valami","nok_aranya")
feor_nem_megoszl$valami<-NULL
feor_nem_megoszl$feor2008_4<-as.factor(feor_nem_megoszl$feor2008_4)
tarifred_11$feor2008_4<-as.factor(tarifred_11$feor2008_4)
tarifred_11<-join(tarifred_11,feor_nem_megoszl,by="feor2008_4",type="left")

tarifred_11$nok_aranya<-as.factor(tarifred_11$nok_aranya)
tarifred_11$kor2 <- tarifred_11$kor^2

regr_11<-lm(formula=lnker~nem +iskveg9 + kor +kor2+szolgho+ ujbel+letszam_bv1  
            + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1,
            data=tarifred_11)
summary(regr_11)
