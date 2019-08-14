# this script contains the A, B and C estimations 
#    what Vincze mentioned in his e-mail on 5th of Aug
# it contains oaxaca and rf estimations only

# „A” változat: csak a fenti változók szerepeljenek (a regresszióban a "kor" négyzet is)

# „B” változat: a fenti változók + feor (2 számjegyű)

# „C” változat: a fenti változók + nők aránya


# R1: szerepel a „nem” a magyarázó változók között

# R2: nem szerepel a „nem” a magyarázó változók között

# sampling
get_sample <- function(df, obs_num){
  index <- sample(seq_len(nrow(df)), obs_num)
  train <- df[index, ]
  test <- df[-index, ]
  list(train=train, test=test)
}

# oaxaca
get_A_oax_regr <- function(df) {
  oax_decomp <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1
                       + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1 - 1
                       | nem
                       , data = df,
                       # Neumark does not include group indicator variable
                       R = 10)
}

get_B_oax_regr <- function(df) {
  oax_decomp <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1
                       + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+ feor_2 - 1
                       | nem
                       , data = df,
                       # Neumark does not include group indicator variable
                       R = 10)
}

get_C_oax_regr <- function(df) {
  oax_decomp <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1
                       + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+ nok_aranya - 1
                       | nem
                       , data = df,
                       # Neumark does not include group indicator variable
                       R = 10)
}


# random forest
get_A_oax_RF_R1 <- function(df, valt_szama, level_szam, num_tree) {
    randfor_ref <- randomForest(lnker ~ nem+iskveg9 +ujbel + kor +szolgho+letszam_bv1
                                + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1,
                                na.action=na.omit, mtry=valt_szama,
                                nodesize=level_szam, ntree=num_tree,
                                importance = F,proximity=F,
                                data=df)
    
}

get_A_oax_RF_R2 <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor+szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df)
}

get_A_oax_RF_female <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female)
  
}

get_A_oax_RF_male <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male)
  
}

get_B_oax_RF_R1 <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- randomForest(lnker ~ nem+iskveg9 +ujbel + kor+szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+ feor_2,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df)
}

get_B_oax_RF_R2 <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1+ feor_2,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df)
  
}


get_B_oax_RF_female <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1+ feor_2,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female)
  
}

get_B_oax_RF_male <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1+ feor_2,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male)
  
}

get_C_oax_RF_R1 <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- randomForest(lnker ~ nem+iskveg9 +ujbel + kor +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1
                              + nok_aranya,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df)
  
}

get_C_oax_RF_R2 <- function(df, valt_szama, level_szam, num_tree) {
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1
                              + nok_aranya,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df)
  
}

get_C_oax_RF_female <- function(df, valt_szama, level_szam, num_tree){
  df_female <- subset(df, nem==0)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1+ nok_aranya,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_female)
  
  
}

get_C_oax_RF_male <- function(df, valt_szama, level_szam, num_tree){
  df_male <- subset(df, nem==1)
  
  randfor_ref <- randomForest(lnker ~ iskveg9 +ujbel + kor
                              +szolgho+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +   kshreg + ttip+szesu_v1+ nok_aranya,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=df_male)
}