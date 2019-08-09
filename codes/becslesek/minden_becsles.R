# becslések függvény

setwd("~/phd/research/Bertarifa4/codes")
source("becslesek.R")

minden_becsles <- function(df,df_next, obs_num, level_szam, num_tree ){
  # minta
    index <- sample(seq_len(nrow(df)), obs_num)
    train <- df[index, ]
    test <- df[-index, ]
    
  # regresszi?
    results_regr <- regr(train,test,df_next)
  # lasso
    results_lasso <- lasso(train,test, df_next)
  # rand forest
    results_randfor <- randforest(train, test, df_next, level_szam, num_tree)

    adott_ev <- cbind(mean(train$ev), results_regr[1,2], results_regr[2,2], results_lasso[1,2],
          results_lasso[2,2], results_randfor[1,2], results_randfor[2,2]
          )
    ket_ev <- cbind(mean(df_next$ev),results_regr[3,2], results_lasso[3,2], results_randfor[3,2])

    # oaxaca becslés a train adatbázisra
    res_oax <- oax(train)
    res_oax_RF <- oax_RF(train) # 5. sor a Neumark becslés

    oaxaca_error <- cbind(mean(df_next$ev), res_oax$twofold$overall[5,2],
                          res_oax$twofold$overall[5,4], res_oax_RF[1,2],
                          res_oax_RF[2,2])

    return(list(mse_adott_evre_becsles=as.numeric(adott_ev),
                mse_ket_evre_becsles=as.numeric(ket_ev),
                oaxaca_becsles=as.numeric(oaxaca_error)))
  
}

oaxaca_becsles <- function(){
  # oaxaca
  res_oax <- oax(train)
  oaxaca_error[2,2] <- res_oax$twofold$overall[5,2] # 5. sor a Neumark becsl?s
  oaxaca_error[2,3] <- res_oax$twofold$overall[5,4]
  
  res_oax_RF <- oax_RF(train)
  oaxaca_error[2,4] <- res_oax_RF[1,2]
  oaxaca_error[2,5] <- res_oax_RF[2,2]
  
}