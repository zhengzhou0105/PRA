## Extract Individual model parameters and fit for plotting----
## Author: Zheng Zhou
## Date: 11/21/2022

## Input------
mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"
# mywd <- paste0(getwd(),"/")
# fileDir <- mywd

batchsize <- 5
level <- "overarching"
ApproList <- c("AIC","BIC","WAIC","LPML","Stacking","PBMA","PBMA_BB")

## Settings---------
DSName <- "simu_ADD"
iter <- 1e3

xADD <- seq(from = 0, to = 5, length.out = iter)

# load utility functions
source(file = paste0(fileDir,"WHALB_model_utilities.R"),
       local = T)
options(bitmapType='cairo')

task_ID <- Sys.getenv("SLURM_ARRAY_TASK_ID")
# task_ID <- 1

batch_ID <- task_ID

## Posterior parameters------
## model posterior parameters
load(
  paste0(mywd,"fitList_",DSName,"_batch_",batch_ID,".rdata")
)
fitList_batch <- eval(parse(text = paste0("fitList_batch_",batch_ID)))
rm(list = paste0("fitList_batch_",batch_ID))

## Weightd RR-----

Weights_batch <- lapply(
  as.list(1:batchsize),function(x){
    df_temp <- read.csv(paste0(mywd,"Weights_",DSName,"_batch_",batch_ID,"_",x,".csv"))
    df_temp <- df_temp %>% select(-1)
  }
)
for(b in 1:batchsize){
  fd_weighted <- get_Weighted(
    fitList =  fitList_batch[[b]],
    df_wts =  Weights_batch[[b]],
    input =  xADD,
    level = level,Index = NULL
  )
  for(a in 1:length(ApproList)){
    Appro <- ApproList[a]
    fd_weighted_appro <- fd_weighted[[a]]
    write.csv(fd_weighted_appro,
              file = paste0(mywd,"RR_",Appro,"_weighted_",level,
                            "_batch_",batch_ID,"_",b,".csv"),
              row.names = F)
  }
}
