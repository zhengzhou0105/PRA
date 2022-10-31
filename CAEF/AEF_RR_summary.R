## Summarize attributable excessive fraction of cancer risk
## based on weighted RR from a selected approach
## Author: Zheng Zhou
## Date: Oct 27 2022

## Input------
# 1 = AIC, 2 = BIC, 3 = WAIC, 4 = LPML, 5= stacking, 6 = PBMA,7 = PBMABB
Appro <- Sys.getenv("SLURM_ARRAY_TASK_ID")                        ## use which approach for averaging

## Settings-------
# mywd <- paste0(getwd(),"/")
# mywd <- "C:/Users/bks01/Downloads/Working/"
# fileDir <- mywd

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

batchsize <- 5
batch <- 200

options(device = "cairo")

## Combine AEF from RR from a selected approach--------
AEF_sum_batch <- vector("list",batch * batchsize)

## *overarching parameters---------
level <- "overarching"

for(t in 1:batch){
  for(b in 1:batchsize){
    AEF_RR <- read.csv(file = paste0(mywd,"AEF_RR_",level,
                                     "_appro_",Appro,
                                     "_batch_",t,"_",b,".csv"))
    AEF_median <- apply(AEF_RR,2,median)
    batchindex <- (t-1)*batchsize + b
    AEF_sum_batch[[batchindex]] <- AEF_median
    rm(AEF_RR)
  }
}
df_AEF_sum <- do.call(rbind,AEF_sum_batch)
Count_bad <- apply(df_AEF_sum,2,function(x){
  sum(x > 1)
})
write.csv(df_AEF_sum,
          paste0(mywd,"df_AEF_RR_",level,
                 "_appro_",Appro,"_median.csv"),
          row.names = F)

write.csv(Count_bad,
          paste0(mywd,"Count_bad_RR_",level,
                 "_appro_",Appro,".csv"))


df_ggplot <- melt(t(as.matrix(df_AEF_sum))) %>% select(-1) %>%
  rename(
    iter = Var2
  ) 

# myplot <- ggplot(data = NULL)+geom_blank()+
#   labs(y = "Density",x="Attributable Fraction")+
#   theme_classic(base_size = 30)
# for(i in 1:max(df_ggplot$iter)){
#   data_iter <- df_ggplot %>% filter(iter == i)
#   myplot <- myplot + 
#     geom_density(data = data_iter,
#                  aes(x = value, y = ..density..),
#                  col = "grey")
#     # geom_histogram(data = data_iter,
#     #                                     aes(x = value, y = ..density..),
#     #                                     fill = "grey",col = "black")
# 
# }

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


png(
  paste0(mywd,"Histogram_AEF_RR_",level,
         "_appro_",Appro,".png"),
  width = 1600,height = 1600
)
print(myplot)
dev.off()

## *study3 parameters----------
level <- "study3"

for(t in 1:batch){
  for(b in 1:batchsize){
    AEF_RR <- read.csv(file = paste0(mywd,"AEF_RR_",level,
                                     "_appro_",Appro,
                                     "_batch_",t,"_",b,".csv"))
    AEF_median <- apply(AEF_RR,2,median)
    batchindex <- (t-1)*batchsize + b
    AEF_sum_batch[[batchindex]] <- AEF_median
  }
}
df_AEF_sum <- do.call(rbind,AEF_sum_batch)
write.csv(df_AEF_sum,
          paste0(mywd,"df_AEF_RR_",level,
                 "_appro_",Appro,"_median.csv"),
          row.names = F)