load("~/kutatas/bertarifa/ws_2016.RData")
mean(minta$train$ev)
mse <- matrix(NA, 4,2)
mse[1:4,1] <- c("train_regr", "train_RF", "test_regr", "test_RF")
# mse
# trainre
# regresszióra
mse[1,2] <- sum(A_oax_regr$reg$reg.pooled.2$residuals^2)/obs_num

# RF-re
mse[2,2] <- sum((A_oax_RF_R2_ref$y-A_oax_RF_R2_ref$predicted)^2)/obs_num

# testre
# regresszióra
mse[3,2] <- sum((minta$test$lnker-predict(A_oax_regr$reg$reg.pooled.1, minta$test))^2)/obs_num

# RF-re
mse[4,2] <- sum((minta$test$lnker-predict(A_oax_RF_R2_ref, minta$test))^2)/obs_num


write.csv(mse, file="mse_2016.csv")