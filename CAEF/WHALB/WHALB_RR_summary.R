## Extract Individual model parameters and fit for plotting
## Author: Zheng Zhou
## Date: 11/21/2022

## Input------
mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"
# mywd <- paste0(getwd(),"/")
# fileDir <- mywd

batchsize <- 5
level <- "overarching"
Appro_ID <- 3

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

ApproList <- c("AIC","BIC","WAIC","LPML","Stacking",
               "PBMA","PBMABB")
Appro <- ApproList[Appro_ID]

## Load Weights----
Weights_batch <- lapply(
  as.list(1:batchsize),function(x){
    df_temp <- read.csv(paste0(mywd,"Weights_",DSName,"_batch_",batch_ID,"_",x,".csv"))
    df_temp <- df_temp %>% select(-1)
  }
)
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
  
  ## summarize pars by model
  # extract pars
  Summary_pars_Linear <- data.frame(
      Median = apply(
        pars_Linear,2,quantile,0.5
      ),
      Q025 = apply(
        pars_Linear,2,quantile,0.025
      ),
      Q975 = apply(
        pars_Linear,2,quantile,0.975
      ),
      Q050 = apply(
        pars_Linear,2,quantile,0.05
      ),
      Q950 = apply(
        pars_Linear,2,quantile,0.95
      ),
      Q100 = apply(
        pars_Linear,2,quantile,0.1
      ),
      Q900 = apply(
        pars_Linear,2,quantile,0.9
      )
  )
  write.csv(
      Summary_pars_Linear,
      paste0(mywd,"Summary_pars_Linear_",level,
             "_batch_",batch_ID,"_",b,
             ".csv"),
      row.names = F
  )
  
  Summary_pars_Power <- data.frame(
    Median = apply(
      pars_Power,2,quantile,0.5
    ),
    Q025 = apply(
      pars_Power,2,quantile,0.025
    ),
    Q975 = apply(
      pars_Power,2,quantile,0.975
    ),
    Q050 = apply(
      pars_Power,2,quantile,0.05
    ),
    Q950 = apply(
      pars_Power,2,quantile,0.95
    ),
    Q100 = apply(
      pars_Power,2,quantile,0.1
    ),
    Q900 = apply(
      pars_Power,2,quantile,0.9
    )
  )
  write.csv(
    Summary_pars_Power,
    paste0(mywd,"Summary_pars_Power_",level,
           "_batch_",batch_ID,"_",b,
           ".csv"),
    row.names = F
  )

  Summary_pars_Expo5 <- data.frame(
    Median = apply(
      pars_Expo5,2,quantile,0.5
    ),
    Q025 = apply(
      pars_Expo5,2,quantile,0.025
    ),
    Q975 = apply(
      pars_Expo5,2,quantile,0.975
    ),
    Q050 = apply(
      pars_Expo5,2,quantile,0.05
    ),
    Q950 = apply(
      pars_Expo5,2,quantile,0.95
    ),
    Q100 = apply(
      pars_Expo5,2,quantile,0.1
    ),
    Q900 = apply(
      pars_Expo5,2,quantile,0.9
    )
  )
  write.csv(
    Summary_pars_Expo5,
    paste0(mywd,"Summary_pars_Expo5_",level,
           "_batch_",batch_ID,"_",b,
           ".csv"),
    row.names = F
  )

  Summary_pars_Hill <- data.frame(
    Median = apply(
      pars_Hill,2,quantile,0.5
    ),
    Q025 = apply(
      pars_Hill,2,quantile,0.025
    ),
    Q975 = apply(
      pars_Hill,2,quantile,0.975
    ),
    Q050 = apply(
      pars_Hill,2,quantile,0.05
    ),
    Q950 = apply(
      pars_Hill,2,quantile,0.95
    ),
    Q100 = apply(
      pars_Hill,2,quantile,0.1
    ),
    Q900 = apply(
      pars_Hill,2,quantile,0.9
    )
  )
  write.csv(
    Summary_pars_Hill,
    paste0(mywd,"Summary_pars_Hill_",level,
           "_batch_",batch_ID,"_",b,
           ".csv"),
    row.names = F
  )
  
  ## fit based on summary of pars

  fd_summary_Linear <- data.frame(
       x = xADD,
       Median = get_Linear(xADD,
                         Summary_pars_Linear$Median[1],
                         Summary_pars_Linear$Median[2]),
       Q025 = get_Linear(xADD,
                         Summary_pars_Linear$Q025[1],
                         Summary_pars_Linear$Q025[2]),
       Q975 = get_Linear(xADD,
                         Summary_pars_Linear$Q975[1],
                         Summary_pars_Linear$Q975[2]),
       Q050 = get_Linear(xADD,
                         Summary_pars_Linear$Q050[1],
                         Summary_pars_Linear$Q050[2]),
       Q950 = get_Linear(xADD,
                         Summary_pars_Linear$Q950[1],
                         Summary_pars_Linear$Q950[2]),
       Q100 = get_Linear(xADD,
                         Summary_pars_Linear$Q100[1],
                         Summary_pars_Linear$Q100[2]),
       Q900 = get_Linear(xADD,
                         Summary_pars_Linear$Q900[1],
                         Summary_pars_Linear$Q900[2])
  )
  write.csv(
    fd_summary_Linear,
    paste0(mywd,"fd_Summary_Linear_",level,
           "_batch_",batch_ID,"_",b,".csv"),
    row.names = F
  )
  
  fd_summary_Power <- data.frame(
    x = xADD,
    Median = get_Power(xADD,
                       Summary_pars_Power$Median[1],
                       Summary_pars_Power$Median[2],
                       Summary_pars_Power$Median[3]),
    Q025 = get_Power(xADD,
                     Summary_pars_Power$Q025[1],
                     Summary_pars_Power$Q025[2],
                     Summary_pars_Power$Q025[3]),
    Q975 = get_Power(xADD,
                     Summary_pars_Power$Q975[1],
                     Summary_pars_Power$Q975[2],
                     Summary_pars_Power$Q975[3]),
    Q050 = get_Power(xADD,
                     Summary_pars_Power$Q050[1],
                     Summary_pars_Power$Q050[2],
                     Summary_pars_Power$Q050[3]),
    Q950 = get_Power(xADD,
                     Summary_pars_Power$Q950[1],
                     Summary_pars_Power$Q950[2],
                     Summary_pars_Power$Q950[3]),
    Q100 = get_Power(xADD,
                     Summary_pars_Power$Q100[1],
                     Summary_pars_Power$Q100[2],
                     Summary_pars_Power$Q100[3]),
    Q900 = get_Power(xADD,
                     Summary_pars_Power$Q900[1],
                     Summary_pars_Power$Q900[2],
                     Summary_pars_Power$Q900[3])
  )
  write.csv(
    fd_summary_Power,
    paste0(mywd,"fd_Summary_Power_",level,
           "_batch_",batch_ID,"_",b,".csv"),
    row.names = F
  )
  
  fd_summary_Expo5 <- data.frame(
    x = xADD,
    Median = get_Expo5(xADD,
                        Summary_pars_Expo5$Median[1],
                       Summary_pars_Expo5$Median[2],
                       Summary_pars_Expo5$Median[3],
                       Summary_pars_Expo5$Median[4]),
    Q025 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q025[1],
                      Summary_pars_Expo5$Q025[2],
                      Summary_pars_Expo5$Q025[3],
                      Summary_pars_Expo5$Q025[4]),
    Q975 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q975[1],
                      Summary_pars_Expo5$Q975[2],
                      Summary_pars_Expo5$Q975[3],
                      Summary_pars_Expo5$Q975[4]),
    Q050 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q050[1],
                      Summary_pars_Expo5$Q050[2],
                      Summary_pars_Expo5$Q050[3],
                      Summary_pars_Expo5$Q050[4]),
    Q950 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q950[1],
                      Summary_pars_Expo5$Q950[2],
                      Summary_pars_Expo5$Q950[3],
                      Summary_pars_Expo5$Q950[4]),
    Q100 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q100[1],
                      Summary_pars_Expo5$Q100[2],
                      Summary_pars_Expo5$Q100[3],
                      Summary_pars_Expo5$Q100[4]),
    Q900 = get_Expo5(xADD,
                      Summary_pars_Expo5$Q900[1],
                      Summary_pars_Expo5$Q900[2],
                      Summary_pars_Expo5$Q900[3],
                      Summary_pars_Expo5$Q900[4])
  )
  write.csv(
    fd_summary_Expo5,
    paste0(mywd,"fd_Summary_Expo5_",level,
           "_batch_",batch_ID,"_",b,".csv"),
    row.names = F
  )
  
  fd_summary_Hill <- data.frame(
    x = xADD,
    Median = get_Hill(xADD,
                    Summary_pars_Hill$Median[1],
                    Summary_pars_Hill$Median[2],
                    Summary_pars_Hill$Median[3],
                    Summary_pars_Hill$Median[4]),
    Q025 = get_Hill(xADD,
                    Summary_pars_Hill$Q025[1],
                    Summary_pars_Hill$Q025[2],
                    Summary_pars_Hill$Q025[3],
                    Summary_pars_Hill$Q025[4]),
    Q975 = get_Hill(xADD,
                    Summary_pars_Hill$Q975[1],
                    Summary_pars_Hill$Q975[2],
                    Summary_pars_Hill$Q975[3],
                    Summary_pars_Hill$Q975[4]),
    Q050 = get_Hill(xADD,
                    Summary_pars_Hill$Q050[1],
                    Summary_pars_Hill$Q050[2],
                    Summary_pars_Hill$Q050[3],
                    Summary_pars_Hill$Q050[4]),
    Q950 = get_Hill(xADD,
                    Summary_pars_Hill$Q950[1],
                    Summary_pars_Hill$Q950[2],
                    Summary_pars_Hill$Q950[3],
                    Summary_pars_Hill$Q950[4]),
    Q100 = get_Hill(xADD,
                    Summary_pars_Hill$Q100[1],
                    Summary_pars_Hill$Q100[2],
                    Summary_pars_Hill$Q100[3],
                    Summary_pars_Hill$Q100[4]),
    Q900 = get_Hill(xADD,
                    Summary_pars_Hill$Q900[1],
                    Summary_pars_Hill$Q900[2],
                    Summary_pars_Hill$Q900[3],
                    Summary_pars_Hill$Q900[4])
  )
  write.csv(
    fd_summary_Hill,
    paste0(mywd,"fd_Summary_Hill_",level,
           "_batch_",batch_ID,"_",b,".csv"),
    row.names = F
  )

  ##  Weighted RR------
  df_weights <- Weights_batch[[b]]
  weights_appro <- df_weights[,Appro_ID]
  
  fd_summary_weighted <- data.frame(
    x = xADD,
    Median = fd_summary_Linear$Median * weights_appro[1]+
      fd_summary_Hill$Median * weights_appro[2]+
      fd_summary_Power$Median * weights_appro[3]+
      fd_summary_Expo5$Median * weights_appro[4],
    Q025 = fd_summary_Linear$Q025 * weights_appro[1]+
      fd_summary_Hill$Q025 * weights_appro[2]+
      fd_summary_Power$Q025 * weights_appro[3]+
      fd_summary_Expo5$Q025 * weights_appro[4],
    Q975 = fd_summary_Linear$Q975 * weights_appro[1]+
      fd_summary_Hill$Q975 * weights_appro[2]+
      fd_summary_Power$Q975 * weights_appro[3]+
      fd_summary_Expo5$Q975 * weights_appro[4],
    Q050 = fd_summary_Linear$Q050 * weights_appro[1]+
      fd_summary_Hill$Q050 * weights_appro[2]+
      fd_summary_Power$Q050 * weights_appro[3]+
      fd_summary_Expo5$Q050 * weights_appro[4],
    Q950 = fd_summary_Linear$Q950 * weights_appro[1]+
      fd_summary_Hill$Q950 * weights_appro[2]+
      fd_summary_Power$Q950 * weights_appro[3]+
      fd_summary_Expo5$Q950 * weights_appro[4],
    Q100 = fd_summary_Linear$Q100 * weights_appro[1]+
      fd_summary_Hill$Q100 * weights_appro[2]+
      fd_summary_Power$Q100 * weights_appro[3]+
      fd_summary_Expo5$Q100 * weights_appro[4],
    Q900 = fd_summary_Linear$Q900 * weights_appro[1]+
      fd_summary_Hill$Q900 * weights_appro[2]+
      fd_summary_Power$Q900 * weights_appro[3]+
      fd_summary_Expo5$Q900 * weights_appro[4]
  )
  write.csv(
    fd_summary_weighted,
    file = paste0(mywd,"fd_Summary_",Appro,"_weighted_",level,
                  "_batch_",batch_ID,"_",b,".csv"),
    row.names = F
  )
}

