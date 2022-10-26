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

## Settings------

task_ID <- Sys.getenv("SLURM_ARRAY_TASK_ID")
# task_ID <- 1

batch_ID <- task_ID

## Utility functions------
get_AEF_CSF <- function(AEI,a){
  if(a <= 0){ stop("prevalence must be positive")}
  if(sum(AEI <= 0) >= 1){stop("Attributable incidence must be postive")}
  
  AEF <- AEI / p
  
  return(AEF)
}

get_AEF_RR <- function(RR){
  if(a <= 0){ stop("prevalence must be positive")}
  if(sum(RR <= 0) >= 1){stop("Relative risk must be positive")}
  
  AEF <- 1 - (1/ RR )
  
  return(AEF)
}

## Load data------

## attritubale incidence by CSF
AEI_CSF_batch <- vector(length = batchsize)
for(b in 1:batchsize){
  AEI_CSF_batch[[b]] <- read.csv(
    file = paste0(mywd,"AEI_CSF_batch_",batch_ID,"_",b,".csv")
  )
}

## weighted RR by BHBMD

# overarching parameters
RR_weighted_overarching_batch <- mapply(
  load,file = paste0(mywd,"RR_weighted_",level,
                     "_batch_",batch_ID,"_",1:batchsize, ".rdata")
)


# study3 parameters
RR_weighted_study3_batch <- mapply(
  load,file = paste0(mywd,"RR_weighted_",level,
                     "_batch_",batch_ID,"_",1:batchsize, ".rdata")
)

## AEF by CSF------
a_bladder <- 0.000005
AEF_CSF_batch <- lapply(AEI_CSF_batch,function(x){
  AEF <- get_AEF_CSF(x,a = a_bladder)
})

## AEF by BHBMD RR-----

# overarching parameters
AEF_RR_overarching <- lapply(RR_weighted_overarching_batch,
                             get_AEF_RR)

AEF_RR_study3 <- lapply(RR_weighted_study3_batch,get_AEF_RR)
