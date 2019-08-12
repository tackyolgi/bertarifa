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

minta <- get_sample(tarifred_08, obs_num)

A_oax_regr <- get_A_oax_regr(minta$train)
B_oax_regr <- get_B_oax_regr(minta$train)
C_oax_regr <- get_C_oax_regr(minta$train)

# R2
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)

  # magyarázott rész
    expl <- mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))-
            mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))

  # nem magyarázott rész
    unexpl <- mean(predict(A_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
              mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))+
              mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))-
              mean(predict(A_oax_regr$reg$reg.B, subset(minta$train, nem==1)))

  # eredmények
    results <- as.data.frame(cbind(modszer="A_R2", nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl))


# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)

  # magyarázott rész
    expl <- mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))-
            mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))

  # nem magyarázott rész
    unexpl <- mean(predict(A_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
              mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))+
              mean(predict(A_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))-
              mean(predict(A_oax_regr$reg$reg.B, subset(minta$test, nem==1)))

  # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="A_R2", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))

# R1_A
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))-
            mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(A_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
              mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))+
              mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))-
              mean(predict(A_oax_regr$reg$reg.B, subset(minta$train, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results, cbind(modszer="A_R1", nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
    
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))-
            mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(A_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
              mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))+
              mean(predict(A_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))-
              mean(predict(A_oax_regr$reg$reg.B, subset(minta$test, nem==1)))
    
    # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="A_R1", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))

    
    

# R2_B
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(B_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))+
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))-
      mean(predict(B_oax_regr$reg$reg.B, subset(minta$train, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results, cbind(modszer="B_R2", nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
    
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(B_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))+
      mean(predict(B_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))-
      mean(predict(B_oax_regr$reg$reg.B, subset(minta$test, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="B_R2", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))
    
# R1_B
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(B_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))+
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))-
      mean(predict(B_oax_regr$reg$reg.B, subset(minta$train, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results, cbind(modszer="B_R1", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
    
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(B_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))+
      mean(predict(B_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))-
      mean(predict(B_oax_regr$reg$reg.B, subset(minta$test, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="B_R1", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))
    
# R2_C
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))
    
    # nem magyarázott rész
    unexpl <- mean(predict(C_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==0)))+
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$train, nem==1)))-
      mean(predict(C_oax_regr$reg$reg.B, subset(minta$train, nem==1)))
    
    # eredmények
    results <- as.data.frame(rbind(results, cbind(modszer="C_R2", nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
    
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(C_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==0)))+
      mean(predict(C_oax_regr$reg$reg.pooled.1, subset(minta$test, nem==1)))-
      mean(predict(C_oax_regr$reg$reg.B, subset(minta$test, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="C_R2", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))
    
# R1_C
# train adatbázison
  # nyers különbség
    raw <- mean(subset(minta$train, nem==0)$lnker)-mean(subset(minta$train, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(C_oax_regr$reg$reg.A, subset(minta$train, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==0)))+
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$train, nem==1)))-
      mean(predict(C_oax_regr$reg$reg.B, subset(minta$train, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results, cbind(modszer="C_R1", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
    
    
# test adatbázison
  # nyers különbség
    raw <- mean(subset(minta$test, nem==0)$lnker)-mean(subset(minta$test, nem==1)$lnker)
    
  # magyarázott rész
    expl <- mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))
    
  # nem magyarázott rész
    unexpl <- mean(predict(C_oax_regr$reg$reg.A, subset(minta$test, nem==0)))-
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==0)))+
      mean(predict(C_oax_regr$reg$reg.pooled.2, subset(minta$test, nem==1)))-
      mean(predict(C_oax_regr$reg$reg.B, subset(minta$test, nem==1)))
    
  # eredmények
    results <- as.data.frame(rbind(results,(cbind(modszer="C_R1", nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, 
                                                  kulonbseg=raw-expl-unexpl))))

# prediction elmentése
    minta$test$A_oax_regr_R1_pred <- predict(A_oax_regr$reg$reg.pooled.2, minta$test)
    minta$test$B_oax_regr_R1_pred <- predict(B_oax_regr$reg$reg.pooled.2, minta$test)
    minta$test$C_oax_regr_R1_pred <- predict(C_oax_regr$reg$reg.pooled.2, minta$test)
    
    minta$test$A_oax_regr_R2_pred <- predict(A_oax_regr$reg$reg.pooled.1, minta$test)
    minta$test$B_oax_regr_R2_pred <- predict(B_oax_regr$reg$reg.pooled.1, minta$test)
    minta$test$C_oax_regr_R2_pred <- predict(C_oax_regr$reg$reg.pooled.1, minta$test)

    
