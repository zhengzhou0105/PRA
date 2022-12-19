## Summarize iterative posterior summary
## Author: Zheng Zhou
## Date: 11/28/2022

## Settings----------
mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

level <- "overarching"

iter <- 1e3
batch <- 200
batchsize <- 5

# model_ID <- Sys.getenv("SLURM_ARRAY_TASK_ID")

modelList <- c("Linear","Power","Expo5","Hill")

DSName <- "simu_ADD"
xADD <- seq(from = 0, to = 5, length.out = iter)

# load utility functions
source(file = paste0(fileDir,"WHALB_model_utilities.R"),
       local = T)
options(bitmapType='cairo')

## Iterative summary------
  
# get model specific data
for(m in 1:length(modelList)){
  model <- modelList[m]
  
  Median_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q025_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q975_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q050_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q950_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q100_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q900_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q250_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  Q750_summary_fd <- matrix(NA,nrow = batchsize * batch,ncol = iter)
  
  for(a in 1:batch){
    
    for(b in 1:batchsize){
        
      Summary_fd <- read.csv(
        # swtich between method 1 and 2
        # paste0(mywd,"Summary_RR_",model,"_",level,
        #        "_batch_",a,"_",b,".csv")
        paste0(mywd,"fd_Summary_",model,"_",level,
               "_batch_",a,"_",b,".csv")
      )
      
      ## Iterative Summary
      index <- (a-1) * batchsize + b
      
      ## median of posterior
      Median_summary_fd[index,] <- Summary_fd[,"Median"]
      ## 95% interval
      Q025_summary_fd[index,] <- Summary_fd[,"Q025"]
      Q975_summary_fd[index,] <- Summary_fd[,"Q975"]
      ## 90% interval
      Q050_summary_fd[index,] <- Summary_fd[,"Q050"]
      Q950_summary_fd[index,] <- Summary_fd[,"Q950"]
      ## 80% interval
      Q100_summary_fd[index,] <- Summary_fd[,"Q100"]
      Q900_summary_fd[index,] <- Summary_fd[,"Q900"]
      # ## 50% interval
      # Q250_summary_fd[index,] <- Summary_fd[,"Q250"]
      # Q750_summary_fd[index,] <- Summary_fd[,"Q750"]
    }
  }
  
  ## output summary by model
  write.csv(
    Median_summary_fd,
    paste0(mywd,"Iter_Median_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q025_summary_fd,
    paste0(mywd,"Iter_Q025_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q975_summary_fd,
    paste0(mywd,"Iter_Q975_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q050_summary_fd,
    paste0(mywd,"Iter_Q050_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q950_summary_fd,
    paste0(mywd,"Iter_Q950_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q100_summary_fd,
    paste0(mywd,"Iter_Q100_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q900_summary_fd,
    paste0(mywd,"Iter_Q900_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q250_summary_fd,
    paste0(mywd,"Iter_Q250_RR_",model,"_",level,".csv"),
    row.names = F
  )
  write.csv(
    Q750_summary_fd,
    paste0(mywd,"Iter_Q750_RR_",model,"_",level,".csv"),
    row.names = F
  )
}

