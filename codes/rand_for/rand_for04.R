setwd("~/phd/research/Bertarifa4/codes")

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
level_szam <- 100    # nodesize


# eredm?nyek m?trix
mse_adott_evre <- matrix(0, 6, 6)
mse_adott_evre <- cbind(c(2011:2016),mse_adott_evre)
mse_adott_evre <- rbind(c("Ev","Regr saj?t mse","Regr test mse",
                             "Lasso saj?t mse", "Lasso test mse",
                             "RF saj?t mse", "RF test mse"), mse_adott_evre)

mse_ket_ev_kozott <- matrix(0, 5, 3)
mse_ket_ev_kozott <- cbind(c(2012:2016),mse_ket_ev_kozott)
mse_ket_ev_kozott <- rbind(c("Becsles eve","Regresszio","Lasso", 
                             "Random Forest"), mse_ket_ev_kozott)

# oaxaca
oaxaca_error <-matrix(0, 6, 4)
oaxaca_error <- cbind(c(2011:2016), oaxaca_error)
oaxaca_error <- rbind(c("Ev","Oaxaca regr exp", "Oaxaca regr unexp"
                        , "Oaxaca RF exp", "Oaxaca RF unexp"), oaxaca_error)


setwd("~/phd/research/Bertarifa4/codes")
source("becslesek.R")

# becsl?sek
  # 2011
    # set.seed(100)
    index <- sample(seq_len(nrow(tarifred_11)), obs_num)
    train <- tarifred_11[index, ]
    test <- tarifred_11[-index, ]

  # regresszi?
    results_regr <- regr(train,test,tarifred_12)
    
    mse_adott_evre[2,2] <- results_regr[1,2]
    mse_adott_evre[2,3] <- results_regr[2,2]
    mse_ket_ev_kozott[2,2] <- results_regr[3,2]
    
  # lasso
    results_lasso <- lasso(train,test, tarifred_12)
      
    mse_adott_evre[2,4] <- results_lasso[1,2]
    mse_adott_evre[2,5] <- results_lasso[2,2]
    mse_ket_ev_kozott[2,3] <- results_lasso[3,2]

  # rand forest
    results_randfor <- randforest(train, test, tarifred_12)
      
    mse_adott_evre[2,6] <- results_randfor[1,2]
    mse_adott_evre[2,7] <- results_randfor[2,2]
    mse_ket_ev_kozott[2,4] <- results_randfor[3,2]
      
  # oaxaca
    res_oax <- oax(train)
    oaxaca_error[2,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
    oaxaca_error[2,3] <- res_oax$twofold$overall[5,4]
    
    res_oax_RF <- oax_RF(train)
    oaxaca_error[2,4] <- res_oax_RF[1,2]
    oaxaca_error[2,5] <- res_oax_RF[2,2]
    
    train = 0
    test = 0
    
# becsl?sek
  # 2012
    
    # set.seed(100)
    index <- sample(seq_len(nrow(tarifred_12)), obs_num)
    train <- tarifred_12[index, ]
    test <- tarifred_12[-index, ]
    
  # regresszi?
    results_regr <- regr(train,test,tarifred_13)
      
    mse_adott_evre[3,2] <- results_regr[1,2]
    mse_adott_evre[3,3] <- results_regr[2,2]
    mse_ket_ev_kozott[3,2] <- results_regr[3,2]
    
  # lasso
    results_lasso <- lasso(train,test, tarifred_13)
      
    mse_adott_evre[3,4] <- results_lasso[1,2]
    mse_adott_evre[3,5] <- results_lasso[2,2]
    mse_ket_ev_kozott[3,3] <- results_lasso[3,2]
    
  # rand forest
    results_randfor <- randforest(train, test, tarifred_13)
      
    mse_adott_evre[3,6] <- results_randfor[1,2]
    mse_adott_evre[3,7] <- results_randfor[2,2]
    mse_ket_ev_kozott[3,4] <- results_randfor[3,2]
  
  # oaxaca
    res_oax <- oax(train)
    oaxaca_error[3,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
    oaxaca_error[3,3] <- res_oax$twofold$overall[5,4]
    
    res_oax_RF <- oax_RF(train)
    oaxaca_error[3,4] <- res_oax_RF[1,2]
    oaxaca_error[3,5] <- res_oax_RF[2,2]
    
   
    train = 0
    test = 0
    
# becsl?sek
  # 2013
    
  # set.seed(100)
  index <- sample(seq_len(nrow(tarifred_13)), obs_num)
  train <- tarifred_13[index, ]
  test <- tarifred_13[-index, ]
    
  # regresszi?
  results_regr <- regr(train,test,tarifred_14)
    
  mse_adott_evre[4,2] <- results_regr[1,2]
  mse_adott_evre[4,3] <- results_regr[2,2]
  mse_ket_ev_kozott[4,2] <- results_regr[3,2]
    
  # lasso
  results_lasso <- lasso(train,test, tarifred_14)
    
  mse_adott_evre[4,4] <- results_lasso[1,2]
  mse_adott_evre[4,5] <- results_lasso[2,2]
  mse_ket_ev_kozott[4,3] <- results_lasso[3,2]
    
  # rand forest
  results_randfor <- randforest(train, test, tarifred_14)
  
  mse_adott_evre[4,6] <- results_randfor[1,2]
  mse_adott_evre[4,7] <- results_randfor[2,2]
  mse_ket_ev_kozott[4,4] <- results_randfor[3,2]

# oaxaca
  res_oax <- oax(train)
  oaxaca_error[4,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
  oaxaca_error[4,3] <- res_oax$twofold$overall[5,4]
  
  res_oax_RF <- oax_RF(train)
  oaxaca_error[4,4] <- res_oax_RF[1,2]
  oaxaca_error[4,5] <- res_oax_RF[2,2]
    
  train = 0
  test = 0
    
# becsl?sek
  # 2014
    
  # set.seed(100)
  index <- sample(seq_len(nrow(tarifred_14)), obs_num)
  train <- tarifred_14[index, ]
  test <- tarifred_14[-index, ]
    
  # regresszi?
  results_regr <- regr(train,test,tarifred_15)
    
  mse_adott_evre[5,2] <- results_regr[1,2]
  mse_adott_evre[5,3] <- results_regr[2,2]
  mse_ket_ev_kozott[5,2] <- results_regr[3,2]
    
  # lasso
  results_lasso <- lasso(train,test, tarifred_15)
    
  mse_adott_evre[5,4] <- results_lasso[1,2]
  mse_adott_evre[5,5] <- results_lasso[2,2]
  mse_ket_ev_kozott[5,3] <- results_lasso[3,2]
    
  # rand forest
  results_randfor <- randforest(train, test, tarifred_15)
    
  mse_adott_evre[5,6] <- results_randfor[1,2]
  mse_adott_evre[5,7] <- results_randfor[2,2]
  mse_ket_ev_kozott[5,4] <- results_randfor[3,2]
  
  # oaxaca
  res_oax <- oax(train)
  oaxaca_error[5,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
  oaxaca_error[5,3] <- res_oax$twofold$overall[5,4]
  
  res_oax_RF <- oax_RF(train)
  oaxaca_error[5,4] <- res_oax_RF[1,2]
  oaxaca_error[5,5] <- res_oax_RF[2,2]

  train = 0
  test = 0
  
# becsl?sek
  # 2015
    
  # set.seed(100)
  index <- sample(seq_len(nrow(tarifred_15)), obs_num)
  train <- tarifred_15[index, ]
  test <- tarifred_15[-index, ]
    
  # regresszi?
  results_regr <- regr(train,test,tarifred_16)
  
  mse_adott_evre[6,2] <- results_regr[1,2]
  mse_adott_evre[6,3] <- results_regr[2,2]
  mse_ket_ev_kozott[6,2] <- results_regr[3,2]
  
  # lasso
  results_lasso <- lasso(train,test, tarifred_16)
  
  mse_adott_evre[6,4] <- results_lasso[1,2]
  mse_adott_evre[6,5] <- results_lasso[2,2]
  mse_ket_ev_kozott[6,3] <- results_lasso[3,2]
  
  # rand forest
  results_randfor <- randforest(train, test, tarifred_16)
  
  mse_adott_evre[6,6] <- results_randfor[1,2]
  mse_adott_evre[6,7] <- results_randfor[2,2]
  mse_ket_ev_kozott[6,4] <- results_randfor[3,2]  
  
  # oaxaca
  res_oax <- oax(train)
  oaxaca_error[6,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
  oaxaca_error[6,3] <- res_oax$twofold$overall[5,4]
  
  res_oax_RF <- oax_RF(train)
  oaxaca_error[6,4] <- res_oax_RF[1,2]
  oaxaca_error[6,5] <- res_oax_RF[2,2]
    
# becsl?sek
  # 2016
  
  set.seed(100)
  index <- sample(seq_len(nrow(tarifred_16)), obs_num)
  train <- tarifred_16[index, ]
  test <- tarifred_16[-index, ]
  
  # regresszi?
  results_regr <- regr(train,test,tarifred_16)
  
  mse_adott_evre[7,2] <- results_regr[1,2]
  mse_adott_evre[7,3] <- results_regr[2,2]
  
  # lasso
  results_lasso <- lasso(train,test, tarifred_16)
  
  mse_adott_evre[7,4] <- results_lasso[1,2]
  mse_adott_evre[7,5] <- results_lasso[2,2]
  
  # rand forest
  results_randfor <- randforest(train, test, tarifred_16)
  
  mse_adott_evre[7,6] <- results_randfor[1,2]
  mse_adott_evre[7,7] <- results_randfor[2,2]
  
    
oaxaca_decomp <- matrix(0, 6,2)
oaxaca_decomp <- cbind(c(2011:2016),oaxaca_decomp)
oaxaca_decomp <- rbind(c("Ev","Explained", "Unexplained"), oaxaca_decomp)
    

    
    
