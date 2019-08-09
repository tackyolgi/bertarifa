library(haven)
# adatbet?lt?s
data_08 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2008_b2.dta")
# data_09 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2009_b2.dta")
# data_10 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2010_b2.dta")
# data_11 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2011_b2.dta")
# data_12 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2012_b2.dta")
# data_13 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2013_b2.dta")
# data_14 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2014_b2.dta")
# data_15 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2015_b2_A.dta")
# data_16 <- read_dta("C:/Users/tacky/Documents/Adat/Adat_bertarifa/tarifa2016_b2_A.dta")


# 2 jegyu feor
data_08$feor_2 <- substr(data_08$feor1997_4, 1,2)

tarifred_08$feor_2 <- substr(tarifred_08$feor1997_4, 1,2)


# alsokas?g
tarifred_08 <- subset(data_08, fforma==1 & atip==4)
# tarifred_09 <- subset(data_09, fforma==1 & atip==4)
# tarifred_10 <- subset(data_10, fforma==1 & atip==4)
# tarifred_11 <- subset(data_11, fforma==1 & atip==4)
# tarifred_12 <- subset(data_12, fforma==1 & atip==4)
# tarifred_13 <- subset(data_13, fforma==1 & atip==4)
# tarifred_14 <- subset(data_14, fforma==1 & atip==4)
# tarifred_15 <- subset(data_15, fforma==1 & atip==4)
# tarifred_16 <- subset(data_16, fforma==1 & atip==4)

# kor n?gyzete
tarifred_08$kor2 <- tarifred_08$kor^2
# tarifred_09$kor2 <- tarifred_09$kor^2
# tarifred_10$kor2 <- tarifred_10$kor^2
# tarifred_11$kor2 <- tarifred_11$kor^2
# tarifred_12$kor2 <- tarifred_12$kor^2
# tarifred_13$kor2 <- tarifred_13$kor^2
# tarifred_14$kor2 <- tarifred_14$kor^2
# tarifred_15$kor2 <- tarifred_15$kor^2
# tarifred_16$kor2 <- tarifred_16$kor^2

# nők megoszlása a 2 jegyű feoroknál
source('C:/Users/tacky/Documents/kutatas/bertarifa/codes/funs/female_percent_in_feor.R', encoding = 'UTF-8')
library(dplyr)

tarifred_08 <- get_female_percent_in_feor97(data_08, tarifred_08)
# tarifred_09 <- get_female_percent_in_feor97(data_09, tarifred_09)
# tarifred_10 <- get_female_percent_in_feor97(data_10, tarifred_10)
# tarifred_11 <- get_female_percent_in_feor08(data_11, tarifred_11)
# tarifred_12 <- get_female_percent_in_feor08(data_12, tarifred_12)
# tarifred_13 <- get_female_percent_in_feor08(data_13, tarifred_13)
# tarifred_14 <- get_female_percent_in_feor08(data_14, tarifred_14)
# tarifred_15 <- get_female_percent_in_feor08(data_15, tarifred_15)
# tarifred_16 <- get_female_percent_in_feor08(data_16, tarifred_16)

# NA n?lk?li sokas?g
source("C:/Users/tacky/Documents/kutatas/bertarifa/codes/funs/torles.R")

tarifred_08 <- delete_missing_obs(tarifred_08)
# tarifred_09 <- delete_missing_obs(tarifred_09)
# tarifred_10 <- delete_missing_obs(tarifred_10)
# tarifred_11 <- delete_missing_obs(tarifred_11)
# tarifred_12 <- delete_missing_obs(tarifred_12)
# tarifred_13 <- delete_missing_obs(tarifred_13)
# tarifred_14 <- delete_missing_obs(tarifred_14)
# tarifred_15 <- delete_missing_obs(tarifred_15)
# tarifred_16 <- delete_missing_obs(tarifred_16)

# factors
source("C:/Users/tacky/Documents/kutatas/bertarifa/codes/funs/factorizalas.R")
tarifred_08 <- get_factors(tarifred_08)
# tarifred_09 <- get_factors(tarifred_09)
# tarifred_10 <- get_factors(tarifred_10)
# tarifred_11 <- get_factors(tarifred_11)
# tarifred_12 <- get_factors(tarifred_12)
# tarifred_13 <- get_factors(tarifred_13)
# tarifred_14 <- get_factors(tarifred_14)
# tarifred_15 <- get_factors(tarifred_15)
# tarifred_16 <- get_factors(tarifred_16)



