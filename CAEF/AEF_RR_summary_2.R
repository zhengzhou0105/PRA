## Summarize attributable excessive fraction of cancer risk
## based on weighted RR from a selected approach
## Author: Zheng Zhou
## Date: Oct 30 2022

## Input------
Appro <- Sys.getenv("SLURM_ARRAY_TASK_ID")

## Settings--------
# mywd <- paste0(getwd(),"/")
# mywd <- "C:/Users/bks01/Downloads/Working/"
# fileDir <- mywd

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

Nappro <- 7
batch <- 200

library("tidyverse")
library("reshape2")

## Overarching parameters-------
level <- "overarching"
median_AEF_batch <- lapply(
  as.list(1:batch),function(x){
    return(read.csv(paste0(mywd,"median_AEF_RR_",level,"_appro_",Appro,
                           "_batch_",x,".csv")))
  }
)
df_AEF <- do.call(rbind,median_AEF_batch)
write.csv(df_AEF,
          file = paste0(mywd,"df_AEF_RR_",level,
                        "_appro_",Appro,".csv"),
          row.names = F)

df_ggplot <- melt(t(as.matrix(df_AEF))) %>% select(-1) %>%
  rename(
    iter = Var2
  ) 

myplot <- ggplot(data = df_ggplot)+ 
  # geom_histogram(aes(x = value,y = ..density.., group = iter),
  #                fill = "white",col = "grey"
  #                # binwidth = 0.1,
  #                # stat = "density"
  # )+
  geom_density(aes(x = value,group = iter, y = ..density..),
               col = "grey")+
  # coord_cartesian(xlim = c(0,500))+
  labs(y ="Density", x="Attributable Fraction")+
  theme_classic(base_size = 20)


svg(
  paste0(mywd,"Histogram_AEF_RR_",level,
         "_appro_",Appro,".svg"),
  width = 16,height = 16
)
print(myplot)
dev.off()

## Study3 parameters--------
level <- "study3"
median_AEF_batch <- lapply(
  as.list(1:batch),function(x){
    return(read.csv(paste0(mywd,"median_AEF_RR_",level,"_appro_",Appro,
                           "_batch_",x,".csv")))
  }
)
df_AEF <- do.call(rbind,median_AEF_batch)
write.csv(df_AEF,
          file = paste0(mywd,"df_AEF_RR_",level,
                        "_appro_",Appro,".csv"),
          row.names = F)

df_ggplot <- melt(t(as.matrix(df_AEF))) %>% select(-1) %>%
  rename(
    iter = Var2
  ) 

myplot <- ggplot(data = df_ggplot)+ 
  # geom_histogram(aes(x = value,y = ..density.., group = iter),
  #                fill = "white",col = "grey"
  #                # binwidth = 0.1,
  #                # stat = "density"
  # )+
  geom_density(aes(x = value,group = iter, y = ..density..),
               col = "grey")+
  # coord_cartesian(xlim = c(0,500))+
  labs(y ="Density", x="Attributable Fraction")+
  theme_classic(base_size = 20)


svg(
  paste0(mywd,"Histogram_AEF_RR_",level,
         "_appro_",Appro,".svg"),
  width = 16,height = 16
)
print(myplot)
dev.off()
