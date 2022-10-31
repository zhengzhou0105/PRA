## Summarize attributable excessive fraction of cancer risk
## based on cancer slope factors
## Author: Zheng Zhou
## Date: Oct 27 2022

## Settings-------
mywd <- paste0(getwd(),"/")
mywd <- "C:/Users/bks01/Downloads/Working/"
fileDir <- mywd

mywd <- "/N/scratch/zhezhou/test/"
fileDir <- "/N/slate/zhezhou/"

batchsize <- 5
batch <- 200

library("reshape2")
library("tidyverse")


## Combine AEF from CSF--------
AEF_CSF_batch <- lapply(
  as.list(1:batch),function(x){
    df_temp <- as.data.frame(read.csv(paste0(mywd,"AEF_CSF_batch_",x,".csv")))
    return(df_temp)
  }
)
df_AEF_CSF <- as.data.frame(do.call(rbind,AEF_CSF_batch))
names(df_AEF_CSF) <- NULL

write.csv(df_AEF_CSF,"df_AEF_CSF.csv",row.names = F)

## Plotting------

## transform dataframe to long for plotting
df_ggplot <- melt(t(as.matrix(df_AEF_CSF))) %>% select(-1) %>%
  rename(
  iter = Var2
) 

Count_bad <- apply(df_AEF_CSF,2,function(x){
  sum(x > 1)
})
write.csv(Count_bad,"Count_bad_CSF.csv",row.names = F)

myplot <- ggplot(data = df_ggplot)+ 
  geom_histogram(aes(x = value,y = ..density.., group = iter),
                 fill = "white",col = "grey"
                 # binwidth = 0.1,
                 # stat = "density"
                 )+
  geom_density(aes(x = value,group = iter, y = ..density..), col = "red")+
  coord_cartesian(xlim = c(0,500))+
  labs(y ="Density", x="Attributable Fraction")+
  theme_classic(base_size = 20)

# myplot2 <- gghistogram(data = df_ggplot,
#                        x = "value",group = "iter", y = "..count..",add = "median", add_density = T)
plotbase <- ggplot(data = NULL)+geom_blank()+
  labs(y = "Density",x="Attributable Fraction")+
  theme_classic(base_size = 30)
for(i in 1:max(df_ggplot$iter)){
  data_iter <- df_ggplot %>% filter(iter == i)
  plotbase <- plotbase + geom_histogram(data = data_iter,
              aes(x = value, y = ..density..),
              fill = "grey",col = "black")
  
}


options(device = "cario")
png(
  paste0(mywd,"AEF_CSF_iter.png"),
  width = 1600,height = 1600
)
print(plotbase)
dev.off()
