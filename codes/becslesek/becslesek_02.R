# becsl?sek

library(oaxaca)
library(randomForest)

# becsl?sek
# mintavétel
get_sample <- function(df, obs_num){
  index <- sample(seq_len(nrow(df)), obs_num)
  train <- df[index, ]
  test <- df[-index, ]
  list(train=train, test=test)
}


# regresszi? ols
lm_regr <- function(df_train, df_test, df_next) {
  regression <- lm(formula=lnker~nem +iskveg9 + kor +kor2+szolgho+ ujbel+letszam_bv1  
                   + ag1+feor_2+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+nok_aranya_sample,
                   data=df_train)
  
  pred_reg <- predict(regression, df_test)
  
  pred_reg_next <- predict(regression, df_next)
  
  list(lm_regr=regression, prediction_test=pred_reg, prediction_next_year=pred_reg_next)
}


# lasso
lasso <- function(df_train, df_test, df_next){
  lasso_regr <- cv.glmnet(cbind(df_train$nem, df_train$iskveg9, df_train$kor, df_train$kor2,
                                df_train$szolgho, df_train$ujbel, df_train$letszam_bv1,
                                df_train$ag1, df_train$feor_2, df_train$ksz, df_train$kag, df_train$kol,
                                df_train$ara, df_train$kra, df_train$kshreg,
                                df_train$ttip, df_train$szesu_v1, df_train$nok_aranya_sample)
                          ,df_train$lnker, 
                          type.measure = "mse", alpha=1, family="gaussian")
  
  
  pred_lasso <- predict(lasso_regr, s=lasso_regr$lambda.1se,
                        newx = cbind(cbind(df_test$nem, df_test$iskveg9, df_test$kor,
                                           df_test$kor2,
                                           df_test$szolgho, df_test$ujbel, df_test$letszam_bv1,
                                           df_test$ag1, df_test$feor_2, df_test$ksz, df_test$kag, 
                                           df_test$kol,
                                           df_test$ara, df_test$kra, df_test$kshreg,
                                           df_test$ttip, df_test$szesu_v1, df_test$nok_aranya_sample)))
  
  
  pred_lasso_next <- predict(lasso_regr, s=lasso_regr$lambda.1se,
                             cbind(df_next$nem, df_next$iskveg9, 
                                   df_next$kor, df_next$kor2,
                                   df_next$szolgho, df_next$ujbel, 
                                   df_next$letszam_bv1, df_next$ag1,
                                   df_next$feor_2,
                                   df_next$ksz, df_next$kag, 
                                   df_next$kol, df_next$ara, 
                                   df_next$kra, df_next$kshreg,
                                   df_next$ttip, df_next$szesu_v1,
                                   df_next$nok_aranya_sample))
  
 
  
  list(lasso_regr=lasso_regr, prediction_test=pred_lasso, prediction_next_year=pred_lasso_next)
  
}

# random forest
randforest <- function(df_train, df_test, df_next, level_szam, num_tree) {
  randfor <- randomForest(lnker ~ nem +iskveg9 + kor 
                          +szolgho+ ujbel+letszam_bv1  
                          + ag1+ feor_2 + ksz+ kag+kol+ ara+ kra
                          +kshreg + ttip+szesu_v1+nok_aranya_sample, 
                          na.action=na.omit, mtry=valt_szama, 
                          nodesize=level_szam, ntree=num_tree, 
                          importance = F,proximity=F,
                          data=df_train)
  
  pred_RF <- predict(randfor, df_test)
  
  pred_RF_next <- predict(randfor, df_next)
  
  list(randfor=randfor, prediction_test=pred_RF, prediction_next_year=pred_RF_next)
  
}

# oaxaca
oax <- function(df, feor){
  if (feor==1){
    oax_decomp <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1
                         + ag1+feor_2+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1 - 1
                         | nem
                         , data = df,
                         # Neumark does not include group indicator variable
                         R = 10)
  } else {
    oax_decomp <- oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1
                         + ag1+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+ nok_aranya_sample
                         - 1
                         | nem
                         , data = df,
                         # Neumark does not include group indicator variable
                         R = 10)
    
  }
  
  oax_decomp
  
}

#  oaxaca RF-fel
oax_RF <- function(df, valt_szama, level_szam, num_tree, feor) {
  if (feor==1){
    randfor_ref <- randomForest(lnker ~ iskveg9 + kor
                                +szolgho+ ujbel+letszam_bv1
                                + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                                +kshreg + ttip+szesu_v1,
                                na.action=na.omit, mtry=valt_szama,
                                nodesize=level_szam, ntree=num_tree,
                                importance = F,proximity=F,
                                data=df)
    Fadat <- subset(df, nem == 0)
    Madat <- subset(df, nem == 1)
    randfor_F <- randomForest(lnker ~ iskveg9 + kor
                              +szolgho+ ujbel+letszam_bv1
                              + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                              +kshreg + ttip+szesu_v1,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=Fadat)
    randfor_M <- randomForest(lnker ~ iskveg9 + kor
                              +szolgho+ ujbel+letszam_bv1
                              + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                              +kshreg + ttip+szesu_v1,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=Madat)
    pred_M_ref <- predict(randfor_ref, Madat)
    pred_F_ref <- predict(randfor_ref, Fadat)
    
    pred_M_M <- predict(randfor_M, Madat)
    pred_F_F <- predict(randfor_F, Fadat)
  } else {
    randfor_ref <- randomForest(lnker ~ iskveg9 + kor
                                +szolgho+ ujbel+letszam_bv1
                                + ag1+ ksz+ kag+kol+ ara+ kra
                                +kshreg + ttip+szesu_v1+nok_aranya_sample,
                                na.action=na.omit, mtry=valt_szama,
                                nodesize=level_szam, ntree=num_tree,
                                importance = F,proximity=F,
                                data=df)
    Fadat <- subset(df, nem == 0)
    Madat <- subset(df, nem == 1)
    randfor_F <- randomForest(lnker ~ iskveg9 + kor
                              +szolgho+ ujbel+letszam_bv1
                              + ag1+  ksz+ kag+kol+ ara+ kra
                              +kshreg + ttip+szesu_v1+nok_aranya_sample,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=Fadat)
    randfor_M <- randomForest(lnker ~ iskveg9 + kor
                              +szolgho+ ujbel+letszam_bv1
                              + ag1+ ksz+ kag+kol+ ara+ kra
                              +kshreg + ttip+szesu_v1+nok_aranya_sample,
                              na.action=na.omit, mtry=valt_szama,
                              nodesize=level_szam, ntree=num_tree,
                              importance = F,proximity=F,
                              data=Madat)
    pred_M_ref <- predict(randfor_ref, Madat)
    pred_F_ref <- predict(randfor_ref, Fadat)
    
    pred_M_M <- predict(randfor_M, Madat)
    pred_F_F <- predict(randfor_F, Fadat)
  }
  

  list(RF_ref=randfor_ref, RF_male=randfor_M, RF_female=randfor_F, 
       pred_M_ref=pred_M_ref, pred_F_ref=pred_F_ref, pred_M_M=pred_M_M, pred_F_F=pred_F_F)
}




