load("~/kutatas/bertarifa/ws_2016.RData")
mean(minta$train$ev)
mse <- matrix(NA, 4,2)
mse[1:4,1] <- c("train_regr", "train_RF", "test_regr", "test_RF")
# mse
# trainre
# regresszióra

mse[1,2] <-   (sum((subset(minta$train, nem==0)$lnker-predict(A_oax_regr$reg$reg.A, subset(minta$train, nem==0)))^2)+
    sum((subset(minta$train, nem==1)$lnker-predict(A_oax_regr$reg$reg.B, subset(minta$train, nem==1)))^2))/
    nrow(minta$train)
 
# RF-re
mse[2,2] <- (sum((subset(minta$train, nem==0)$lnker-predict(A_oax_RF_female, subset(minta$train, nem==0)))^2)+
  sum((subset(minta$train, nem==1)$lnker-predict(A_oax_RF_male, subset(minta$train, nem==1)))^2))/
  nrow(minta$train)


# testre
# regresszióra
mse[3,2] <- (sum((subset(minta$test, nem==0)$lnker-predict(A_oax_regr$reg$reg.A, subset(minta$test, nem==0)))^2)+
  sum((subset(minta$test, nem==1)$lnker-predict(A_oax_regr$reg$reg.B, subset(minta$test, nem==1)))^2))/
  nrow(minta$test)

# RF-re
mse[4,2] <- (sum((subset(minta$test, nem==0)$lnker-predict(A_oax_RF_female, subset(minta$test, nem==0)))^2)+
  sum((subset(minta$test, nem==1)$lnker-predict(A_oax_RF_male, subset(minta$test, nem==1)))^2))/
  nrow(minta$test)


write.csv(mse, file="mse2_2016.csv")