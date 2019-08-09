# becsl?sek

library(oaxaca)
library(randomForest)

# becsl?sek
# regresszi? ols
regr <- function(df_train, df_test, df_next) {
  res <- matrix(0,3,1)
  regression <- lm(formula=lnker~nem +iskveg9 + kor +kor2+szolgho+ ujbel+letszam_bv1  
                   + ag1+feor_2+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+nok_aranya,
                   data=df_train)
  
  res[1,1] <- mean((regression$fitted.values-regression$model$lnker)^2)
  
  pred_reg <- predict(regression, df_test)
  res[2,1] <- mean((df_test$lnker-pred_reg)^2, na.rm=TRUE)
  
  pred_reg_next <- predict(regression, df_next)
  res[3,1] <- mean((df_next$lnker-pred_reg_next)^2)
  cbind(c("train mse", "test mse", "k?vetkez? ?v mse"), res)
}


# lasso
lasso <- function(df_train, df_test, df_next){
  res <- matrix(0,3,1)
  lasso_regr <- cv.glmnet(cbind(df_train$nem, df_train$iskveg9, df_train$kor, df_train$kor2,
                                df_train$szolgho, df_train$ujbel, df_train$letszam_bv1,
                                df_train$ag1, df_train$feor_2, df_train$ksz, df_train$kag, df_train$kol,
                                df_train$ara, df_train$kra, df_train$kshreg,
                                df_train$ttip, df_train$szesu_v1, df_train$nok_aranya)
                          ,df_train$lnker, 
                          type.measure = "mse", alpha=1, family="gaussian")
  
  res[1,1] <- lasso_regr$cvm[lasso_regr$lambda == lasso_regr$lambda.1se]
  
  pred_lasso <- predict(lasso_regr, s=lasso_regr$lambda.1se,
                        newx = cbind(cbind(df_test$nem, df_test$iskveg9, df_test$kor,
                                           df_test$kor2,
                                           df_test$szolgho, df_test$ujbel, df_test$letszam_bv1,
                                           df_test$ag1, df_test$feor_2, df_test$ksz, df_test$kag, 
                                           df_test$kol,
                                           df_test$ara, df_test$kra, df_test$kshreg,
                                           df_test$ttip, df_test$szesu_v1, df_test$nok_aranya)))
  
  res[2,1] <- mean((df_test$lnker-pred_lasso)^2)
  
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
                                   df_next$nok_aranya))
  
  res[3,1] <- mean((df_next$lnker-pred_lasso_next)^2)
  
  cbind(c("train mse", "test mse", "k?vetkez? ?v mse"), res)
  
}

# random forest
randforest <- function(df_train, df_test, df_next, level_szam, num_tree) {
  res <- matrix(0,3,1)
  randfor <- randomForest(lnker ~ nem +iskveg9 + kor 
                           +szolgho+ ujbel+letszam_bv1  
                           + ag1+ feor_2 + ksz+ kag+kol+ ara+ kra
                           +kshreg + ttip+szesu_v1+nok_aranya, 
                           na.action=na.omit, mtry=valt_szama, 
                           nodesize=level_szam, ntree=num_tree, 
                           importance = F,proximity=F,
                           data=df_train)

  res[1,1] <- mean((randfor$y-randfor$predicted)^2)

  pred_RF <- predict(randfor, df_test)

  res[2,1] <- mean((df_test$lnker-pred_RF)^2)

  pred_RF_next <- predict(randfor, df_next)

  res[3,1] <- mean((df_next$lnker-pred_RF_next)^2)
  
  cbind(c("train mse", "test mse", "k?vetkez? ?v mse"), res)
}

# oaxaca
oax <- function(df){
  oaxaca(formula = lnker~  iskveg9 +ujbel + kor +kor2+szolgho+letszam_bv1  
         + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra+   kshreg + ttip+szesu_v1+ nok_aranya
         - 1
         | nem 
         , data = df, 
         # Neumark does not include group indicator variable
         R = 10)
}

#  oaxaca RF-fel
oax_RF <- function(df) {
  randfor_ref <- randomForest(lnker ~ iskveg9 + kor 
                          +szolgho+ ujbel+letszam_bv1  
                          + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                          +kshreg + ttip+szesu_v1+nok_aranya, 
                          na.action=na.omit, mtry=valt_szama, 
                          nodesize=level_szam, ntree=num_tree, 
                          importance = F,proximity=F,
                          data=df)
  Fadat <- subset(df, nem == 0)
  Madat <- subset(df, nem == 1)
  randfor_F <- randomForest(lnker ~ iskveg9 + kor 
                            +szolgho+ ujbel+letszam_bv1  
                            + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                            +kshreg + ttip+szesu_v1+nok_aranya, 
                            na.action=na.omit, mtry=valt_szama, 
                            nodesize=level_szam, ntree=num_tree, 
                            importance = F,proximity=F,
                            data=Fadat)
  randfor_M <- randomForest(lnker ~ iskveg9 + kor 
                            +szolgho+ ujbel+letszam_bv1  
                            + ag1+ feor_2+ ksz+ kag+kol+ ara+ kra
                            +kshreg + ttip+szesu_v1+nok_aranya, 
                            na.action=na.omit, mtry=valt_szama, 
                            nodesize=level_szam, ntree=num_tree, 
                            importance = F,proximity=F,
                            data=Madat)
  pred_M_ref <- predict(randfor_ref, Madat)
  pred_F_ref <- predict(randfor_ref, Fadat)
  
  res <- matrix(0,3,1)
  res <- cbind(c("exp","unexp","osszes"), res)
  
  res[1,2] <- mean(pred_F_ref)-mean(pred_M_ref)
  
  pred_M_M <- predict(randfor_M, Madat)
  pred_F_F <- predict(randfor_F, Fadat)
  
  res[2,2] <- mean(pred_M_ref-pred_M_M)+mean(pred_F_F-pred_F_ref)
  res[3,2] <- mean(pred_F_F)-mean(pred_M_M)
  res
}




