## Summarize attributable excessive fraction of cancer risk
## based on weighted RR from a selected approach
## Author: Zheng Zhou
## Date: Oct 27 2022

## Input------
# 1 = AIC, 2 = BIC, 3 = WAIC, 4 = LPML, 5= stacking, 6 = PBMA,7 = PBMABB
# Appro <- Sys.getenv("SLURM_ARRAY_TASK_ID")                        ## use which approach for averaging
batch_id <- Sys.getenv("SLURM_ARRAY_TASK_ID")

## Settings-------
# mywd <- paste0(getwd(),"/")
# mywd <- "C:/Users/bks01/Downloads/Working/"
# fileDir <- mywd

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

Nappro <- 7
batchsize <- 5
iter <- 1e3

## Combine AEF from RR from a selected approach--------
# AEF_sum_batch <- vector("list",batch * batchsize)
AEF_sum_arr <- array(NA,dim=c(Nappro,batchsize,iter))

## *overarching parameters---------
level <- "overarching"

for(a in 1:Nappro){
  AEF_b <- vector("list",batchsize)
  for(b in 1:batchsize){
    AEF <- read.csv(
      paste0(mywd,"AEF_RR_",level,
             "_appro_",a,
             "_batch_",batch_id,"_",b,".csv")
    )
    AEF_median <- apply(AEF,2,median)
    AEF_b[[b]] <- AEF_median
    rm(AEF)
  }
  df_AEF <- do.call(rbind,AEF_b)
  write.csv(
    df_AEF,
    file = paste0(mywd,"median_AEF_RR_",level,"_appro_",a,
                  "_batch_",batch_id,".csv"),
    row.names = F
  )
}


## *study3 parameters----------
level <- "study3"

for(a in 1:Nappro){
  AEF_b <- vector("list",batchsize)
  for(b in 1:batchsize){
    AEF <- read.csv(
      paste0(mywd,"AEF_RR_",level,
             "_appro_",a,
             "_batch_",batch_id,"_",b,".csv")
    )
    AEF_median <- apply(AEF,2,median)
    AEF_b[[b]] <- AEF_median
    rm(AEF)
  }
  df_AEF <- do.call(rbind,AEF_b)
  write.csv(
    df_AEF,
    file = paste0(mywd,"median_AEF_RR_",level,"_appro_",a,
                  "_batch_",batch_id,".csv"),
    row.names = F
  )
}