# „A” változat: csak a fenti változók szerepeljenek (a regresszióban a "kor" négyzet is)

# „B” változat: a fenti változók + feor (2 számjegyű)

# „C” változat: a fenti változók + nők aránya


# R1: szerepel a „nem” a magyarázó változók között

# R2: nem szerepel a „nem” a magyarázó változók között

library(tidyverse)

source('~/kutatas/bertarifa/codes/data_loading.R', encoding = 'UTF-8')

obs_num <- 60000

# parameters
# random forest
num_tree <- 250      # ntree
valt_szama <- 5      # mtry
level_szam <- 50    # nodesize

# becslesek
library(oaxaca)
library(randomForest)

source('~/kutatas/bertarifa/codes/becslesek/becslesek_03.R', encoding = 'UTF-8')

minta <- get_sample(tarifred_16, obs_num)

A_oax_regr <- get_A_oax_regr(minta$train)