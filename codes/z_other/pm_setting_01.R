setwd("~/phd/research/Bertarifa4/codes")

source("data_loading.R")

library(tictoc)
library(randomForest) #tuneRF-hez kell
library(DAAG)
library(plyr)

obs_num <- 20000


# nker~nem +iskveg9 + kor +kor2+szolgho+ ujbel+letszam_bv1  
#+ ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1

tarifred_11$teaor2008_3<-as.factor(tarifred_11$teaor2008_3)
tarifred_11$feor2008_4<-as.factor(tarifred_11$feor2008_4)
tarifred_11$kor2 <- tarifred_11$kor^2
minta <- tarifred_15_red[sample(nrow(tarifred_15_red), obs_num),]


tic()
model <- tuneRF( cbind(minta$nem, minta$iskveg9, minta$kor, #minta$kor2,
                      minta$szolgho, minta$ujbel, minta$letszam_bv1,
                      minta$ag1, minta$ksz, minta$kag, minta$kol,
                      minta$ara, minta$kra, minta$kshreg,minta$ttip, 
                      minta$szesu_v1),minta$lnker,
                      ntreeTry = 300, 
                      stepFactor = 1, 
                      improve = 0.01, 
                      trace=TRUE, plot=TRUE)
toc()

# parameters
  # random forest
    num_tree <- 500      # ntree
    valt_szama <- 4      # mtry
    level_szam <- 10    # nodesize
    
minta <- tarifred_15_red[sample(nrow(tarifred_15_red), obs_num),]
    
# random forest
    tic("rand forest")
    randfor <- randomForest(lnker ~ iskveg9+ letszam_bv1 + kra + 
                                 kor+ ag1+ + kol+ ara+ szolgho
                                 + kshreg +nem+ ujbel+ ttip+szesu_v1, 
                                 na.action=na.omit, mtry=valt_szama, 
                                 nodesize=level_szam, ntree=num_tree, 
                                 importance = T,proximity=T,
                                 data=minta)
    toc()
    
    plot(randfor, sub="year 2015")
