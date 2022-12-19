## Extract Individual model parameters and fit for plotting
## Author: Zheng Zhou
## Date: 11/21/2022

## Input------
level <- "overarching"

ApproList <- c("AIC","BIC","WAIC","LPML","Stacking","PBMA","PBMA_BB")

## Settings---------
iter <- 1e3

DSName <- "simu_ADD"

xADD <- seq(from = 0, to = 5, length.out = iter)

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"
# mywd <- paste0(getwd(),"/")
# fileDir <- mywd

batchsize <- 5

# load utility functions
source(file = paste0(fileDir,"WHALB_model_utilities.R"),
       local = T)
options(bitmapType='cairo')

task_ID <- Sys.getenv("SLURM_ARRAY_TASK_ID")
# task_ID <- 1

batch_ID <- task_ID

## Summarize weighted RR--------
for(b in 1:batchsize){
  for(a in 1:length(ApproList)){
  Appro <- ApproList[a]
  
    # load weighted posterior RR
    fd_weighted <- read.csv(
      paste0(mywd,"RR_",Appro,"_weighted_",level,
             "_batch_",batch_ID,"_",b,".csv")
      
    )
    
    # summarize over simulation
    fd_summary_weighted <- data.frame(
      xADD = xADD,
      Median = apply(fd_weighted,2,quantile,0.5),
      Q250 = apply(fd_weighted,2,quantile,0.25),
      Q750 = apply(fd_weighted,2,quantile,0.75),
      Q100 = apply(fd_weighted,2,quantile,0.1),
      Q900 = apply(fd_weighted,2,quantile,0.9),
      Q050 = apply(fd_weighted,2,quantile,0.05),
      Q950 = apply(fd_weighted,2,quantile,0.95),
      Q025 = apply(fd_weighted,2,quantile,0.025),
      Q975 = apply(fd_weighted,2,quantile,0.975)
    )
    
    # output summary
    write.csv(
      fd_summary_weighted,
      file = paste0(mywd,"Summary_RR_",Appro,"_weighted_",level,
                    "_batch_",batch_ID,"_",b,".csv")
    )
  }
}
