# egy másik kódban megírva a becslések
# 	nyers különbség: férfi átlag – női átlag
#   magyarázott rész
#   nem magyarázott rész
#   nyers különbség - magyarázott – nem magyarázott
#   ezt megnézni ols-nél: Oaxaca beta-diff a különbség, 
#   mean(X_A*beta_A)- mean(X_B*beta_B) nem megmagyarázott rész a test adatbázison, 
#       ez lesz a bias
#   Y ̅_A-Y ̅_B=X ̅_A β_A-X ̅_B β_B ez nem fog teljesülni

setwd("~/phd/research/Bertarifa4/codes")

library(tidyverse)

source("data_loading.R")

library(tictoc)
library(DAAG)
library(plyr)
library(glmnet)


obs_num <- 60000

# parameters
# random forest
num_tree <- 250      # ntree
valt_szama <- 5      # mtry
level_szam <- 10    # nodesize




setwd("~/phd/research/Bertarifa4/codes")
source("becslesek_02.R")

# minta
# minta <- get_sample(tarifred_08, obs_num)

# rmodel_lm <- lm_regr(minta$train, minta$test, tarifred_09)
# rmodel_lasso <- lasso(minta$train, minta$test, tarifred_09)
# rmodel_rf <- randforest(minta$train, minta$test, tarifred_09, level_szam, num_tree)

minta <- get_sample(tarifred_08, obs_num)
# oaxnál, ha a második argumentum 1-akkor feor-ok vannak a becslésben, ha nem, akkor a nők aránya
rmodel_oax_regr_train <- oax(minta$train, 0)


# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)

  # magyarázott rész
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$train, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$train, nem==1)))

  # nem magyarázott rész
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(minta$train, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$train, nem==0)))+
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$train, nem==1)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(minta$train, nem==1)))
  
  # eredmények
    results <- as.data.frame(cbind(nyers=raw, magyarazott=expl, 
                       nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl))


# test adatbázison
    # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
    # magyarázott rész
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$test, nem==0)))-
              mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$test, nem==1)))
    
    # nem magyarázott rész
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(minta$test, nem==0)))-
                mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$test, nem==0)))+
                mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(minta$test, nem==1)))-
                mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(minta$test, nem==1)))
    
    # eredmények
    results <- as.data.frame(rbind(results,(cbind(nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))

tic()
rmodel_oax_RF_train <- oax_RF(minta$train, valt_szama, level_szam, num_tree,0)
toc()
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)

  # magyarázott rész
    expl <- mean(rmodel_oax_RF_train$pred_F_ref)-
      mean(rmodel_oax_RF_train$pred_M_ref)

  # nem magyarázott rész
    unexpl <- mean(rmodel_oax_RF_train$pred_F_F)-mean(rmodel_oax_RF_train$pred_F_ref)+
      mean(rmodel_oax_RF_train$pred_M_ref)-mean(rmodel_oax_RF_train$pred_M_M)
    
  # eredmények
    results <- as.data.frame(rbind(results,(cbind(nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(rmodel_oax_RF_train$RF_ref, subset(minta$test, nem==0)))-
      mean(predict(rmodel_oax_RF_train$RF_ref, subset(minta$test, nem==1)))
    
    # nem magyarázott rész
    unexpl <- mean(predict(rmodel_oax_RF_train$RF_female, subset(minta$test, nem==0)))-
      mean(predict(rmodel_oax_RF_train$RF_ref, subset(minta$test, nem==0)))+
      mean(predict(rmodel_oax_RF_train$RF_ref, subset(minta$test, nem==1)))-
      mean(predict(rmodel_oax_RF_train$RF_male, subset(minta$test, nem==1)))
    
    # eredmények
    results <- as.data.frame(rbind(results,(cbind(nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))
    
    
