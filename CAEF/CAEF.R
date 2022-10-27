## Compare attributable cancer risk factors calculated from
## cancer slope factor and relative risk
## Author: Zheng Zhou
## Date: Oct 26 2022

## Input-----
# mywd <- paste0(getwd(),"/")
# mywd <- "C:/Users/bks01/Downloads/Working/"
# fileDir <- mywd

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

DSName <- "simu_ADD"
batchsize <- 5
batch <- 200

a_bladder <- 3.6e-5              # ASR by Chinese population 

## Settings------

task_ID <- Sys.getenv("SLURM_ARRAY_TASK_ID")
# task_ID <- 1

batch_ID <- task_ID

## Utility functions------
get_AEF_CSF <- function(AEI,a){
  if(a <= 0){ stop("prevalence must be positive")}
  if(sum(AEI <= 0) >= 1){stop("Attributable incidence must be postive")}
  
  AEF <- AEI / a
  
  return(AEF)
}

get_AEF_RR <- function(RR){
  
  if(sum(RR <= 0) >= 1){stop("Relative risk must be positive")}
  
  AEF <- 1 - (1/ RR )
  
  return(AEF)
}

## AEF by CSF------

## attritubale incidence by CSF
AEI_CSF_batch <- vector(length = batchsize)
for(b in 1:batchsize){
  AEI_CSF_batch[[b]] <- read.csv(
    file = paste0(mywd,"AEI_CSF_batch_",batch_ID,"_",b,".csv")
  )
}

assign(
  paste0("AEF_CSF_batch_",batch_ID),
  lapply(AEI_CSF_batch,function(x){
    AEF <- get_AEF_CSF(x,a = a_bladder)
  })
)
save(list = paste0("AEF_CSF_batch_",batch_ID),
     file = paste0(mywd,"AEF_CSF_batch_",batch_ID,".rdata"))

## AEF by BHBMD RR-----

## each data contains RR weighted by 7 approaches

# *overarching parameters-------
level <- "overarching"
for(b in 1:batchsize){
  load(file = paste0(mywd,"RR_weighted_",level,"_batch_",
                     batch_ID,"_",b, ".rdata"))
  assign(
    paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    lapply(fd_weighted,get_AEF_RR)
  )
  save(
    list = paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    file = paste0(mywd,"AEF_RR_",level,"_batch_",batch_ID,"_",b,".rdata")
  )
} 

# *study3 parameters----------
level <- "study3"
for(b in 1:batchsize){
  load(file = paste0(mywd,"RR_weighted_",level,"_batch_",
                     batch_ID,"_",b, ".rdata"))
  assign(
    paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    lapply(fd_weighted,get_AEF_RR)
  )
  save(
    list = paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    file = paste0(mywd,"AEF_RR_",level,"_batch_",batch_ID,"_",b,".rdata")
  )
} 