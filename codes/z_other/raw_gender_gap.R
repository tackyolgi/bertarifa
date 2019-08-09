# raw gender wage gap 

setwd("~/phd/research/Adat_bertarifa")


library(haven)
# adatbet?lt?s
data_86 <- read_dta("tarifa1986_b2.dta")
data_89 <- read_dta("tarifa1989_b2.dta")
data_92 <- read_dta("tarifa1992_b2.dta")
data_93 <- read_dta("tarifa1993_b2.dta")
data_94 <- read_dta("tarifa1994_b2.dta")
data_95 <- read_dta("tarifa1995_b2.dta")
data_96 <- read_dta("tarifa1996_b2.dta")
data_97 <- read_dta("tarifa1997_b2.dta")
data_98 <- read_dta("tarifa1998_b2.dta")
data_99 <- read_dta("tarifa1999_b2.dta")
data_00 <- read_dta("tarifa2000_b2.dta")
data_01 <- read_dta("tarifa2001_b2.dta")
data_02 <- read_dta("tarifa2002_b2.dta")
data_03 <- read_dta("tarifa2003_b2.dta")
data_04 <- read_dta("tarifa2004_b2.dta")
data_05 <- read_dta("tarifa2005_b2.dta")
data_06 <- read_dta("tarifa2006_b2.dta")
data_07 <- read_dta("tarifa2007_b2.dta")
data_08 <- read_dta("tarifa2008_b2.dta")
data_09 <- read_dta("tarifa2009_b2.dta")
data_10 <- read_dta("tarifa2010_b2.dta")
data_11 <- read_dta("tarifa2011_b2.dta")
data_12 <- read_dta("tarifa2012_b2.dta")
data_13 <- read_dta("tarifa2013_b2.dta")
data_14 <- read_dta("tarifa2014_b2.dta")
data_15 <- read_dta("tarifa2015_b2_A.dta")
data_16 <- read_dta("tarifa2016_b2_A.dta")

# alsokas?g
tarifred_86 <- subset(data_86, fforma==1 & atip==4)
tarifred_89 <- subset(data_89, fforma==1 & atip==4)
tarifred_92 <- subset(data_92, fforma==1 & atip==4)
tarifred_93 <- subset(data_93, fforma==1 & atip==4)
tarifred_94 <- subset(data_94, fforma==1 & atip==4)
tarifred_95 <- subset(data_95, fforma==1 & atip==4)
tarifred_96 <- subset(data_96, fforma==1 & atip==4)
tarifred_97 <- subset(data_97, fforma==1 & atip==4)
tarifred_98 <- subset(data_98, fforma==1 & atip==4)
tarifred_99 <- subset(data_99, fforma==1 & atip==4)
tarifred_00 <- subset(data_00, fforma==1 & atip==4)
tarifred_01 <- subset(data_01, fforma==1 & atip==4)
tarifred_02 <- subset(data_02, fforma==1 & atip==4)
tarifred_03 <- subset(data_03, fforma==1 & atip==4)
tarifred_04 <- subset(data_04, fforma==1 & atip==4)
tarifred_05 <- subset(data_05, fforma==1 & atip==4)
tarifred_06 <- subset(data_06, fforma==1 & atip==4)
tarifred_07 <- subset(data_07, fforma==1 & atip==4)
tarifred_08 <- subset(data_08, fforma==1 & atip==4)
tarifred_09 <- subset(data_09, fforma==1 & atip==4)
tarifred_10 <- subset(data_10, fforma==1 & atip==4)
tarifred_11 <- subset(data_11, fforma==1 & atip==4)
tarifred_12 <- subset(data_12, fforma==1 & atip==4)
tarifred_13 <- subset(data_13, fforma==1 & atip==4)
tarifred_14 <- subset(data_14, fforma==1 & atip==4)
tarifred_15 <- subset(data_15, fforma==1 & atip==4)
tarifred_16 <- subset(data_16, fforma==1 & atip==4)

# kor n?gyzete
tarifred_03$kor2 <- tarifred_03$kor^2
tarifred_04$kor2 <- tarifred_04$kor^2
tarifred_05$kor2 <- tarifred_05$kor^2
tarifred_06$kor2 <- tarifred_06$kor^2
tarifred_07$kor2 <- tarifred_07$kor^2
tarifred_08$kor2 <- tarifred_08$kor^2
tarifred_09$kor2 <- tarifred_09$kor^2
tarifred_10$kor2 <- tarifred_10$kor^2
tarifred_11$kor2 <- tarifred_11$kor^2
tarifred_12$kor2 <- tarifred_12$kor^2
tarifred_13$kor2 <- tarifred_13$kor^2
tarifred_14$kor2 <- tarifred_14$kor^2
tarifred_15$kor2 <- tarifred_15$kor^2
tarifred_16$kor2 <- tarifred_16$kor^2

# nők megoszlása a 2 jegyű feoroknál
source('~/phd/research/Bertarifa4/codes/funs/female_percent_in_feor.R', encoding = 'UTF-8')
library(dplyr)
tarifred_03 <- get_female_percent_in_feor97(data_03, tarifred_03)
tarifred_04 <- get_female_percent_in_feor97(data_04, tarifred_04)
tarifred_05 <- get_female_percent_in_feor97(data_05, tarifred_05)
tarifred_06 <- get_female_percent_in_feor97(data_06, tarifred_06)
tarifred_07 <- get_female_percent_in_feor97(data_07, tarifred_07)
tarifred_08 <- get_female_percent_in_feor97(data_08, tarifred_08)
tarifred_09 <- get_female_percent_in_feor97(data_09, tarifred_09)
tarifred_10 <- get_female_percent_in_feor97(data_10, tarifred_10)
tarifred_11 <- get_female_percent_in_feor08(data_11, tarifred_11)
tarifred_12 <- get_female_percent_in_feor08(data_12, tarifred_12)
tarifred_13 <- get_female_percent_in_feor08(data_13, tarifred_13)
tarifred_14 <- get_female_percent_in_feor08(data_14, tarifred_14)
tarifred_15 <- get_female_percent_in_feor08(data_15, tarifred_15)
tarifred_16 <- get_female_percent_in_feor08(data_16, tarifred_16)

# NA n?lk?li sokas?g
setwd("~/phd/research/Bertarifa4/codes/funs")
source("torles.R")
tarifred_03 <- delete_missing_obs(tarifred_03)
tarifred_04 <- delete_missing_obs(tarifred_04)
tarifred_05 <- delete_missing_obs(tarifred_05)
tarifred_06 <- delete_missing_obs(tarifred_06)
tarifred_07 <- delete_missing_obs(tarifred_07)
tarifred_08 <- delete_missing_obs(tarifred_08)
tarifred_09 <- delete_missing_obs(tarifred_09)
tarifred_10 <- delete_missing_obs(tarifred_10)
tarifred_11 <- delete_missing_obs(tarifred_11)
tarifred_12 <- delete_missing_obs(tarifred_12)
tarifred_13 <- delete_missing_obs(tarifred_13)
tarifred_14 <- delete_missing_obs(tarifred_14)
tarifred_15 <- delete_missing_obs(tarifred_15)
tarifred_16 <- delete_missing_obs(tarifred_16)

# factors
setwd("~/phd/research/Bertarifa4/codes/funs")
source("factorizalas.R")
tarifred_03 <- get_factors(tarifred_03)
tarifred_04 <- get_factors(tarifred_04)
tarifred_05 <- get_factors(tarifred_05)
tarifred_06 <- get_factors(tarifred_06)
tarifred_07 <- get_factors(tarifred_07)
tarifred_08 <- get_factors(tarifred_08)
tarifred_09 <- get_factors(tarifred_09)
tarifred_10 <- get_factors(tarifred_10)
tarifred_11 <- get_factors(tarifred_11)
tarifred_12 <- get_factors(tarifred_12)
tarifred_13 <- get_factors(tarifred_13)
tarifred_14 <- get_factors(tarifred_14)
tarifred_15 <- get_factors(tarifred_15)
tarifred_16 <- get_factors(tarifred_16)


obs_num <- 20000
setwd("~/phd/research/Bertarifa4/codes")
source("becslesek_02.R")

minta <- get_sample(tarifred_03, obs_num)
rmodel_oax_regr_train <- oax(tarifred_03, 0)
# train adatbazison
  # nyers kulonbseg
    raw <- mean(subset(tarifred_03, nem==0)$lnker)-mean(subset(tarifred_03, nem==1)$lnker)

  # magyarazott resz
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_03, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_03, nem==1)))

  # nem magyarazott resz
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(tarifred_03, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_03, nem==0)))+
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_03, nem==1)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(tarifred_03, nem==1)))

  # eredmenyek
    results <- as.data.frame(cbind(nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl))
# 2004
    minta <- get_sample(tarifred_04, obs_num)
    rmodel_oax_regr_train <- oax(tarifred_04, 0)
    # train adatbazison
    # nyers kulonbseg
    raw <- mean(subset(tarifred_04, nem==0)$lnker)-mean(subset(tarifred_04, nem==1)$lnker)
    
    # magyarazott resz
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_04, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_04, nem==1)))
    
    # nem magyarazott resz
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(tarifred_04, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_04, nem==0)))+
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_04, nem==1)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(tarifred_04, nem==1)))
    
    # eredmenyek
    results <- as.data.frame(rbind(results, cbind(nyers=raw, magyarazott=expl, 
                                   nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
# 2005
    minta <- get_sample(tarifred_05, obs_num)
    rmodel_oax_regr_train <- oax(tarifred_05, 0)
    # train adatbazison
    # nyers kulonbseg
    raw <- mean(subset(tarifred_05, nem==0)$lnker)-mean(subset(tarifred_05, nem==1)$lnker)
    
    # magyarazott resz
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_05, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_05, nem==1)))
    
    # nem magyarazott resz
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(tarifred_05, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_05, nem==0)))+
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_05, nem==1)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(tarifred_05, nem==1)))
    
    # eredmenyek
    results <- as.data.frame(rbind(results, cbind(nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
# 2006
    minta <- get_sample(tarifred_06, obs_num)
    rmodel_oax_regr_train <- oax(tarifred_06, 0)
    # train adatbazison
    # nyers kulonbseg
    raw <- mean(subset(tarifred_06, nem==0)$lnker)-mean(subset(tarifred_06, nem==1)$lnker)
    
    # magyarazott resz
    expl <- mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_06, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_06, nem==1)))
    
    # nem magyarazott resz
    unexpl <- mean(predict(rmodel_oax_regr_train$reg$reg.A, subset(tarifred_06, nem==0)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_06, nem==0)))+
      mean(predict(rmodel_oax_regr_train$reg$reg.pooled.1, subset(tarifred_06, nem==1)))-
      mean(predict(rmodel_oax_regr_train$reg$reg.B, subset(tarifred_06, nem==1)))
    
    # eredmenyek
    results <- as.data.frame(rbind(results, cbind(nyers=raw, magyarazott=expl, 
                                                  nem_magyarazott=unexpl, kulonbseg=raw-expl-unexpl)))
# egy�b
setwd("~/phd/research/Bertarifa4/codes/funs")
source("get_wage_gap.R")
    raw_wage_gap <- cbind(ev_86=get_raw_gap(data_86), ev_89=get_raw_gap(data_89),
                          ev_92=get_raw_gap(data_92), ev_92=get_raw_gap(data_92),
                          ev_93=get_raw_gap(data_93), ev_94=get_raw_gap(data_94),
                          ev_95=get_raw_gap(data_95), ev_96=get_raw_gap(data_96),
                          ev_97=get_raw_gap(data_97), ev_98=get_raw_gap(data_98),
                          ev_99=get_raw_gap(data_99), ev_00=get_raw_gap(data_00),
                          ev_01=get_raw_gap(data_01), ev_02=get_raw_gap(data_02),
                          ev_03=get_raw_gap(data_03), ev_04=get_raw_gap(data_04),
                          ev_05=get_raw_gap(data_05), ev_06=get_raw_gap(data_06),
                          ev_07=get_raw_gap(data_07), ev_08=get_raw_gap(data_08),
                          ev_09=get_raw_gap(data_09), ev_10=get_raw_gap(data_10),
                          ev_11=get_raw_gap(data_11), ev_12=get_raw_gap(data_12),
                          ev_13=get_raw_gap(data_13), ev_14=get_raw_gap(data_14),
                          ev_15=get_raw_gap(data_15), ev_16=get_raw_gap(data_16)
                          )
