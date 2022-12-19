## Extract Individual model parameters and fit for plotting
## Author: Zheng Zhou
## Date: 11/21/2022

## Input------
DSName <- "simu_ADD"
iter <- 1e3
xADD <- seq(from = 0, to = 5, length.out = iter)

level <- "overarching"

modelList <- c("Linear","Power","Expo5","Hill")

## Settings---------
iter <- 1e3

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

# Summarize individual RR----
for(b in 1:batchsize){
  
  for(m in 1:length(modelList)){
  model <- modelList[m]
    # load posterior RRs
    df_fd <- read.csv(
      paste0(mywd,"RR_",model,"_",level,
             "_batch_",batch_ID,"_",b,".csv")
    )
    
    # fd_Linear <- read.csv(
    #   paste0(mywd,"fd_Linear_",level,
    #          "_batch_",batch_ID,"_",b,".csv")
    # )
    # fd_Power <- read.csv(
    #   paste0(mywd,"fd_Power_",level,
    #          "_batch_",batch_ID,"_",b,".csv")
    # )
    # fd_Expo5 <- read.csv(
    #   paste0(mywd,"fd_Expo5_",level,
    #          "_batch_",batch_ID,"_",b,".csv")
    # )
    # fd_Hill <- read.csv(
    #   paste0(mywd,"fd_Hill_",level,
    #          "_batch_",batch_ID,"_",b,".csv")
    # )
    
    # summarize over simulation
    Summary_fd <- data.frame(
      xADD = xADD,
      Median = apply(df_fd,2,quantile,0.5),
      Q250 = apply(df_fd,2,quantile,0.25),
      Q750 = apply(df_fd,2,quantile,0.75),
      Q100 = apply(df_fd,2,quantile,0.1),
      Q900 = apply(df_fd,2,quantile,0.9),
      Q050 = apply(df_fd,2,quantile,0.05),
      Q950 = apply(df_fd,2,quantile,0.95),
      Q025 = apply(df_fd,2,quantile,0.025),
      Q975 = apply(df_fd,2,quantile,0.975)
    )
    
    # Summary_fd_Linear <- data.frame(
    #   xADD = xADD,
    #   Median = apply(fd_Linear,2,quantile,0.5),
    #   Q250 = apply(fd_Linear,2,quantile,0.25),
    #   Q750 = apply(fd_Linear,2,quantile,0.75),
    #   Q100 = apply(fd_Linear,2,quantile,0.1),
    #   Q900 = apply(fd_Linear,2,quantile,0.9),
    #   Q050 = apply(fd_Linear,2,quantile,0.05),
    #   Q950 = apply(fd_Linear,2,quantile,0.95),
    #   Q025 = apply(fd_Linear,2,quantile,0.025),
    #   Q975 = apply(fd_Linear,2,quantile,0.975)
    # )
    # Summary_fd_Hill <- data.frame(
    #   xADD = xADD,
    #   Median = apply(fd_Hill,2,quantile,0.5),
    #   Q250 = apply(fd_Hill,2,quantile,0.25),
    #   Q750 = apply(fd_Hill,2,quantile,0.75),
    #   Q100 = apply(fd_Hill,2,quantile,0.1),
    #   Q900 = apply(fd_Hill,2,quantile,0.9),
    #   Q050 = apply(fd_Hill,2,quantile,0.05),
    #   Q950 = apply(fd_Hill,2,quantile,0.95),
    #   Q025 = apply(fd_Hill,2,quantile,0.025),
    #   Q975 = apply(fd_Hill,2,quantile,0.975)
    # )
    # Summary_fd_Power <- data.frame(
    #   xADD = xADD,
    #   Median = apply(fd_Power,2,quantile,0.5),
    #   Q250 = apply(fd_Power,2,quantile,0.25),
    #   Q750 = apply(fd_Power,2,quantile,0.75),
    #   Q100 = apply(fd_Power,2,quantile,0.1),
    #   Q900 = apply(fd_Power,2,quantile,0.9),
    #   Q050 = apply(fd_Power,2,quantile,0.05),
    #   Q950 = apply(fd_Power,2,quantile,0.95),
    #   Q025 = apply(fd_Power,2,quantile,0.025),
    #   Q975 = apply(fd_Power,2,quantile,0.975)
    # )
    # Summary_fd_Expo5 <- data.frame(
    #   xADD = xADD,
    #   Median = apply(fd_Expo5,2,quantile,0.5),
    #   Q250 = apply(fd_Expo5,2,quantile,0.25),
    #   Q750 = apply(fd_Expo5,2,quantile,0.75),
    #   Q100 = apply(fd_Expo5,2,quantile,0.1),
    #   Q900 = apply(fd_Expo5,2,quantile,0.9),
    #   Q050 = apply(fd_Expo5,2,quantile,0.05),
    #   Q950 = apply(fd_Expo5,2,quantile,0.95),
    #   Q025 = apply(fd_Expo5,2,quantile,0.025),
    #   Q975 = apply(fd_Expo5,2,quantile,0.975)
    # )
    
    ## Output summary for individual
    write.csv(
      Summary_fd,
      paste0(mywd,"Summary_RR_",model,"_",level,
             "_batch_",batch_ID,"_",b,".csv"),
      row.names = F
    )  
      
    # write.csv(
    #   Summary_fd_Linear,
    #   file = paste0(mywd,"Summary_fd_Linear_",level,
    #                 "_batch_",batch_ID,"_",b,".csv")
    # )
    # write.csv(
    #   Summary_fd_Power,
    #   file = paste0(mywd,"Summary_fd_Power_",level,
    #                 "_batch_",batch_ID,"_",b,".csv")
    # )
    # write.csv(
    #   Summary_fd_Expo5,
    #   file = paste0(mywd,"Summary_fd_Expo5_",level,
    #                 "_batch_",batch_ID,"_",b,".csv")
    # )
    # write.csv(
    #   Summary_fd_Hill,
    #   file = paste0(mywd,"Summary_fd_Hill_",level,
    #                 "_batch_",batch_ID,"_",b,".csv")
    # )
    
  }
}
