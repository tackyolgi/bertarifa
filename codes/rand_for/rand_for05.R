# oaxaca dekompozíció a regresszióknál

setwd("~/phd/research/Bertarifa4/codes")

source("data_loading.R")

library(oaxaca)

results <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1  
                  + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1
                  - 1
                  | nem 
                  , data = tarifred_11, 
                   # Neumark does not include group indicator variable
                  R = 10)