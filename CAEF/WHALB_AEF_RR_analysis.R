## Analyze and interpret 
## the excessive fraction of bladder cancer incidence attributable to
## iAs exposure via rice intake in the Chinese urban population
## estimated by WHALB approach
## Author: Zheng Zhou
## Date: Oct 31 2022

## Settings----
ApproList <- c("AIC","BIC","WAIC","LPML","Stacking","PBMA","PBMA_BB")
library("tidyverse")

## AEF_CSF--------
Source <- "CSF"
level <- "overarching"
Appro <- 1

tag <- ifelse(Source =="RR", paste0(Source,"_",level,"_appro_",Appro),
              Source)
df_median_AEF <- read.csv(
  paste0("df_AEF_",tag,".csv")
)

## AEF_RR--------
Source <- "RR"
level <- "study3"

tag <- lapply(as.list(1:7),function(x){
  ifelse(Source =="RR", paste0(Source,"_",level,"_appro_",x),
         Source)
})
df_median_AEF <- lapply(tag,function(x){
  read.csv(
    paste0("df_AEF_",x,".csv"))
})

## Analysis-------
list_AEF_sum <- lapply(df_median_AEF,function(x){
  dist_median <- apply(x,2,median)
  dist_Q025 <- apply(x,2,function(x) quantile(x,0.025))
  dist_Q975 <- apply(x,2,function(x) quantile(x,0.975))
  dist_Q05 <- apply(x,2,function(x) quantile(x,0.05))
  dist_Q95 <- apply(x,2,function(x) quantile(x,0.95))
  
  sum_vec <- c(AEF_mean = mean(dist_median),
               AEF = quantile(dist_median,0.025),
               AEF = quantile(dist_median,0.975),
               AEFU_mean = mean(dist_Q975),
               AEFU = quantile(dist_Q975,0.025),
               AEFU = quantile(dist_Q975,0.975)
               )
  return(sum_vec)
})
df_AEF_sum <- do.call(rbind,list_AEF_sum)
rownames(df_AEF_sum) <- ApproList
colnames(df_AEF_sum) <- c("AEF_mean","AEF_025","AEF_975",
                          "AEFU_mean","AEFU_025","AEFU_975")
df_AEF_sum <- round(df_AEF_sum * 100,2)
write.csv(df_AEF_sum,
          paste0("AEF_RR_",level,"_by_approach.csv"))
