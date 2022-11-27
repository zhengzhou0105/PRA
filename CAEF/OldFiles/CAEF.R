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
Napproach <- 7
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
  AEF[AEF < 0] <- 0
  
  return(AEF)
}

Conv_list_to_df <- function(list_batch){
  batchsize <- length(list_batch)
  df_output <- matrix(unlist(list_batch),nrow = batchsize,byrow = T)
  return(df_output)
}
## AEF by CSF------

## attritubale incidence by CSF
AEI_CSF_batch <- vector(length = batchsize)
for(b in 1:batchsize){
  AEI_CSF_batch[[b]] <- read.csv(
    file = paste0(mywd,"AEI_CSF_batch_",batch_ID,"_",b,".csv")
  )
}

# assign(
#   paste0("AEF_CSF_batch_",batch_ID),
#   lapply(AEI_CSF_batch,function(x){
#     AEF <- get_AEF_CSF(x,a = a_bladder)
#   })
# )
AEF_CSF_batch <- lapply(AEI_CSF_batch,function(x){
  AEF <- get_AEF_CSF(x,a = a_bladder)
  return(AEF)
})

AEF_CSF_df <- Conv_list_to_df(AEF_CSF_batch)


# save(list = paste0("AEF_CSF_batch_",batch_ID),
#      file = paste0(mywd,"AEF_CSF_batch_",batch_ID,".rdata"))
write.csv(
  x = AEF_CSF_df,
  file= paste0(mywd,"AEF_CSF_batch_",batch_ID,".csv"),
  row.names = F
)
## AEF by BHBMD RR-----

## each data contains RR weighted by 7 approaches

# *overarching parameters-------
level <- "overarching"
  for(b in 1:batchsize){
    load(file = paste0(mywd,"RR_weighted_",level,"_batch_",
                       batch_ID,"_",b, ".rdata"))
    for(a in 1:Napproach){
      fd_weighted_app <- fd_weighted[[a]]
      mtx_AEF_RR_app <- get_AEF_RR(fd_weighted_app)
      write.csv(x = mtx_AEF_RR_app,
                file = paste0(mywd,"AEF_RR_",level,"_appro_",a,
                              "_batch_",batch_ID,"_",b,".csv"),
                row.names = F
                )
    }
    # assign(
    #   paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    #   lapply(fd_weighted,get_AEF_RR)
    # )
    # save(
    #   list = paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
    #   file = paste0(mywd,"AEF_RR_",level,"_batch_",batch_ID,"_",b,".rdata")
    # )
  } 

# *study3 parameters----------
level <- "study3"
for(b in 1:batchsize){
  load(file = paste0(mywd,"RR_weighted_",level,"_batch_",
                     batch_ID,"_",b, ".rdata"))
  for(a in 1:Napproach){
    fd_weighted_app <- fd_weighted[[a]]
    mtx_AEF_RR_app <- get_AEF_RR(fd_weighted_app)
    write.csv(x = mtx_AEF_RR_app,
              file = paste0(mywd,"AEF_RR_",level,"_appro_",a,
                            "_batch_",batch_ID,"_",b,".csv"),
              row.names = F
    )
  }
  # assign(
  #   paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
  #   lapply(fd_weighted,get_AEF_RR)
  # )
  # save(
  #   list = paste0("AEF_RR_",level,"_batch_",batch_ID,"_",b),
  #   file = paste0(mywd,"AEF_RR_",level,"_batch_",batch_ID,"_",b,".rdata")
  # )
} 
