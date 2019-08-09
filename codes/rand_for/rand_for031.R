# fa nagyságának ellenõrzése
# változók
# mtry=5


setwd("~/phd/research/Bertarifa4/codes")

source("data_loading.R")

library(tictoc)
library(randomForest) #tuneRF-hez kell
library(DAAG)
library(plyr)


# kb. 20 perc a futási idõ
obs_num <- 60000

# parameters
# random forest
num_tree <- 250      # ntree
valt_szama <- 5      # mtry
level_szam <- 100    # nodesize

set.seed(100)
index <- sample(seq_len(nrow(tarifred_11)), obs_num)
train <- tarifred_11[index, ]
test <- tarifred_11[-index, ]

tic()
randfor_11 <- randomForest(lnker ~ nem +iskveg9 + kor 
                           +szolgho+ ujbel+letszam_bv1  
                           + ag1+ ksz+ kag+kol+ ara+ kra
                           +kshreg + ttip+szesu_v1, 
                           na.action=na.omit, mtry=valt_szama, 
                           nodesize=level_szam, ntree=num_tree, 
                           importance = F,proximity=F,
                           data=train)
toc()
plot(randfor_11)