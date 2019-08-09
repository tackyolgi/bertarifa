delete_missing_obs <- function(df){
  df <- df[!is.na(df$lnker) 
           & !is.na(df$nem)
           & !is.na(df$iskveg9)
           & !is.na(df$kor)
           & !is.na(df$szolgho)
           & !is.na(df$ujbel)
           & !is.na(df$letszam_bv1)
           & !is.na(df$ag1)
           & !is.na(df$feor_2)
           & !is.na(df$ksz)
           & !is.na(df$kag)
           & !is.na(df$kol)
           & !is.na(df$ara)
           & !is.na(df$kra)
           & !is.na(df$kshreg)
           & !is.na(df$ttip)
           & !is.na(df$szesu_v1)
           & !is.na(df$nok_aranya)
           
           ,]
}