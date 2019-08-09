get_female_percent_in_feor97 <- function(df_full, df_sample){
  megoszlas_full <- table(df_full$feor_2, df_full$nem)
  megoszlas_full <- as.data.frame.matrix(megoszlas_full)
  megoszlas_full <- cbind(feor_2=as.integer(rownames(megoszlas_full)),
                          nok_full=as.integer(megoszlas_full[,1]),
                          ferfiak_full=as.integer(megoszlas_full[,2]),
                          nok_aranya=round(megoszlas_full[,1]/
                                             (megoszlas_full[,1]+megoszlas_full[,2]), 4)*100)
  megoszlas_full<-as.data.frame.matrix(megoszlas_full)
  
  megoszlas_sample <- table(df_sample$feor_2, df_sample$nem)
  megoszlas_sample <- as.data.frame.matrix(megoszlas_sample)
  megoszlas_sample <- cbind(feor_2=as.integer(rownames(megoszlas_sample)),
                            nok_sample=as.integer(megoszlas_sample[,1]),
                            ferfiak_sample=as.integer(megoszlas_sample[,2]))
  megoszlas_sample <- as.data.frame.matrix(megoszlas_sample)
  
  megoszlas_full <- left_join(megoszlas_full, megoszlas_sample,
                              by=("feor_2"="feor_2"))
  
  megoszlas_full <- as.data.frame(megoszlas_full)
  
  df_sample$feor_2<-as.integer(df_sample$feor_2)
  
  df_sample <- left_join(df_sample,megoszlas, by=c("feor_2"="feor_2"))
  
}