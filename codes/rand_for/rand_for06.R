# egy másik kódban megírva a becslések
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
level_szam <- 100    # nodesize

# eredm?nyek m?trix
mse_adott_evre <- matrix(0, 1, 7)
mse_adott_evre[1,] <- c("Ev","Regr saj?t mse","Regr test mse",
                          "Lasso saj?t mse", "Lasso test mse",
                          "RF saj?t mse", "RF test mse")

mse_ket_ev_kozott <- matrix(0, 1, 4)
mse_ket_ev_kozott <- c("Becsles eve","Regresszio","Lasso", 
                             "Random Forest")

# oaxaca
oaxaca_compare <-matrix(0, 1, 4)
oaxaca_compare <- c("Ev","Oaxaca regr exp", "Oaxaca regr unexp"
                        , "Oaxaca RF exp", "Oaxaca RF unexp")

setwd("~/phd/research/Bertarifa4/codes")
source("minden_becsles.R")

# 2008-as adatból hiányzik az ag1=15, így 2009-re nem tud becslést csinálni
# a torles.R-ben a 15-ös ágazatot is kitöröltem
  # 2008
    eredmeny <- minden_becsles(tarifred_08,tarifred_09, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2009
    eredmeny <- minden_becsles(tarifred_09,tarifred_10, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2010
    eredmeny <- minden_becsles(tarifred_10,tarifred_11, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2011
    eredmeny <- minden_becsles(tarifred_11,tarifred_12, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2012
    eredmeny <- minden_becsles(tarifred_12,tarifred_13, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2013
    eredmeny <- minden_becsles(tarifred_13,tarifred_14, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2014
    eredmeny <- minden_becsles(tarifred_14,tarifred_15, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2015
    eredmeny <- minden_becsles(tarifred_15,tarifred_16, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
  # 2016 
    # két év között nincs becslés, az ugyanúgy 2016-ra készült
    eredmeny <- minden_becsles(tarifred_16,tarifred_16, obs_num, level_szam, num_tree)
    mse_adott_evre <- rbind(mse_adott_evre, as.numeric(eredmeny$mse_adott_evre_becsles))
    mse_ket_ev_kozott <- rbind(mse_ket_ev_kozott, as.numeric(eredmeny$mse_ket_evre_becsles))
    oaxaca_compare <- rbind(oaxaca_compare, as.numeric(eredmeny$oaxaca_becsles))
    
    
    

