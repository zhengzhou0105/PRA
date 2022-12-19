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

## Individual RR-------
for(b in 1:batchsize){
  fitList <- fitList_batch[[b]]
  pars_Linear <- get_BHBMD_posterior(fitList,level = level,
                                     model = "Linear")
  pars_Power <- get_BHBMD_posterior(fitList,level = level,
                                    model = "Power")
  pars_Expo5 <- get_BHBMD_posterior(fitList,level = level,
                                    model = "Expo5")
  pars_Hill <- get_BHBMD_posterior(fitList,level = level,
                                   model = "Hill")
  iterEff <- nrow(pars_Linear)
  fd_Linear <- matrix(NA,nrow = iterEff,ncol = iter)
  fd_Power <- matrix(NA,nrow = iterEff,ncol = iter)
  fd_Expo5 <- matrix(NA,nrow = iterEff,ncol = iter)
  fd_Hill <- matrix(NA,nrow = iterEff,ncol = iter)
  
  for(i in 1:iterEff){
    fd_Linear[i,] <- get_Linear(xADD,pars_Linear[i,1],pars_Linear[i,2])
    fd_Power[i,] <- get_Power(xADD,pars_Power[i,1],pars_Power[i,2],
                              pars_Power[i,3])
    fd_Expo5[i,] <- get_Expo5(xADD,pars_Expo5[i,1],pars_Expo5[i,2],
                              pars_Expo5[i,3],pars_Expo5[i,4])
    fd_Hill[i,] <- get_Hill(xADD,pars_Hill[i,1],pars_Hill[i,2],pars_Hill[i,3],
                            pars_Hill[i,4])
  }
  write.csv(fd_Linear,
            file = paste0(mywd,"RR_Linear_",level,"_batch_",batch_ID,"_",b,".csv"),
            row.names = F)
  write.csv(fd_Power,
            file = paste0(mywd,"RR_Power_",level,"_batch_",batch_ID,"_",b,".csv"),
            row.names = F)
  write.csv(fd_Expo5,
            file = paste0(mywd,"RR_Expo5_",level,"_batch_",batch_ID,"_",b,".csv"),
            row.names = F)
  write.csv(fd_Hill,
            file = paste0(mywd,"RR_Hill_",level,"_batch_",batch_ID,"_",b,".csv"),
            row.names = F)
  
  
}
