library(oaxaca)

setwd("~/phd/research/Bertarifa4/data")


data <- read.csv("tarifa2015_b2.csv")

tarifred <- subset(data, fforma==1 & atip==4)

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

# dummies
tarifred$iskveg9_07 <- 0
tarifred$iskveg9_07[tarifred$iskveg9==1] <- 1

# oaxaca
results <- oaxaca(formula = lnker ~  
                  + iskveg9
                  + letszam_bv1 
                  + kra 
                  + kor
                  + ag1
                  + exp
                  + kol
                  + ara
                  + szolgho
                  + kshreg
                  + ttip
                  + ujbel
                  - 1
                  | nem
                  , data = tarifred, R = 10)



# group.weight mutatja, hogy melyik csoport a benchmark
# 0 = group A
# 1 = group B
plot(results
     , decomposition = "twofold"
     , group.weight = -1)
                                                                                          