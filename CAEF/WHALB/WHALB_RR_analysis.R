## Analyze and interpret 
## the excessive fraction of bladder cancer incidence attributable to
## iAs exposure via rice intake in the Chinese urban population
## estimated by WHALB approach
## Author: Zheng Zhou
## Date: Oct 31 2022

## Settings----
mywd <- paste0(getwd(),"/")
fileDir <- mywd

ApproList <- c("AIC","BIC","WAIC","LPML","Stacking",
               "PBMA","PBMA_BB")
modelList <- c("Linear","Power","Expo5","Hill")

level <- "overarching"

iter <- 1e3
batch <- 200
batchsize <- 5

DSName <- "simu_ADD"
xADD <- seq(from = 0, to = 5, length.out = iter)

library("tidyverse")

model_ID <- 4
Appro_ID <- 4

## Plot summary of iterative Curves--------

### Load raw data--------
df_raw <- read.csv(
  paste0(fileDir,"Metadata_bladder_iAs_censored.csv")
)
dose <- read.csv(
  paste0(fileDir,"DataList_medianADD.csv")
) %>% select(dose) 
dose <- unlist(dose)

DataList <- df_raw %>% mutate(
  dose = dose
) 
DataList$RR.l <- ifelse(DataList$RR.l == 0,1,DataList$RR.l)
DataList$RR.u <- ifelse(DataList$RR.u == 0,1,DataList$RR.u)

### Individual curves------

model <- modelList[model_ID]

# read summary data
df_Median <- read.csv(
  paste0(mywd,"Iter_Median_RR_",model,"_",level,".csv")
)
df_Q025 <- read.csv(
  paste0(mywd,"Iter_Q025_RR_",model,"_",level,".csv")
)
df_Q975 <- read.csv(
  paste0(mywd,"Iter_Q975_RR_",model,"_",level,".csv")
)
df_Q050 <- read.csv(
  paste0(mywd,"Iter_Q050_RR_",model,"_",level,".csv")
)
df_Q950 <- read.csv(
  paste0(mywd,"Iter_Q950_RR_",model,"_",level,".csv")
)
df_Q100 <- read.csv(
  paste0(mywd,"Iter_Q100_RR_",model,"_",level,".csv")
)
df_Q900 <- read.csv(
  paste0(mywd,"Iter_Q900_RR_",model,"_",level,".csv")
)
df_Q250 <- read.csv(
  paste0(mywd,"Iter_Q250_RR_",model,"_",level,".csv")
)
df_Q750 <- read.csv(
  paste0(mywd,"Iter_Q750_RR_",model,"_",level,".csv")
)

df_plot <- data.frame(
  Median = apply(df_Median,2,quantile,0.5),
  low_median = apply(df_Median,2,quantile,0.025),
  up_median = apply(df_Median,2,quantile,0.975),
  median_low = apply(df_Q050,2,quantile,0.5),
  median_up = apply(df_Q950,2,quantile,0.5),
  x = xADD
)

myplot <- ggplot(df_plot,aes(x = x))+
  geom_line(aes(y = median_low),col = "brown",
            linetype = "dotted",size = 1)+
  # geom_line(aes(y = median_up),col = "red",
  #           linetype = "dotted",size = 1)+
  # geom_line(aes(y = low_median), col = "brown",
  # linetype = "dotted",size = 1)+
  geom_line(aes(y = up_median), col = "brown",
            linetype = "dotted",size = 1)+
  geom_line(aes(y = Median),size = 1.5, col = "blue")

myplot1 <- myplot + 
  geom_point(data = DataList,
             aes(x = dose,y = RR, group = Study,col = Study,
                 shape = Study),
             size =2)+
  scale_shape_manual(values = 1:10)+
  geom_errorbar(data = DataList,
                aes(x = dose,ymin = RR.l,ymax = RR.u,
                    group = Study,col = Study))+
  scale_y_continuous(breaks = 1:15)+
  labs(x = "ADD(ug/kg/day)",
       y = "Relative Risk")+
  theme_classic(base_size = 25)+
  coord_cartesian(ylim = c(0,15),xlim = c(0,5))

ggsave(
  plot= myplot1,
  dev ="svg",
  filename = paste0(mywd,"Iterative_Summary_",model,"_",level,".svg"),
  width = 16,height = 16
)

### Weighted curves-------
Appro <- ApproList[Appro_ID]

# read summary data
df_Median <- read.csv(
  paste0(mywd,"Iter_Median_RR_",Appro,"_weighted_",level,".csv")
)
df_Q025 <- read.csv(
  paste0(mywd,"Iter_Q025_RR_",Appro,"_weighted_",level,".csv")
)
df_Q975 <- read.csv(
  paste0(mywd,"Iter_Q975_RR_",Appro,"_weighted_",level,".csv")
)
df_Q050 <- read.csv(
  paste0(mywd,"Iter_Q050_RR_",Appro,"_weighted_",level,".csv")
)
df_Q950 <- read.csv(
  paste0(mywd,"Iter_Q950_RR_",Appro,"_weighted_",level,".csv")
)
df_Q100 <- read.csv(
  paste0(mywd,"Iter_Q100_RR_",Appro,"_weighted_",level,".csv")
)
df_Q900 <- read.csv(
  paste0(mywd,"Iter_Q900_RR_",Appro,"_weighted_",level,".csv")
)
df_Q250 <- read.csv(
  paste0(mywd,"Iter_Q250_RR_",Appro,"_weighted_",level,".csv")
)
df_Q750 <- read.csv(
  paste0(mywd,"Iter_Q750_RR_",Appro,"_weighted_",level,".csv")
)

df_plot <- data.frame(
  Median = apply(df_Median,2,quantile,0.5),
  low_median = apply(df_Median,2,quantile,0.025),
  up_median = apply(df_Median,2,quantile,0.975),
  median_low = apply(df_Q050,2,quantile,0.5),
  median_up = apply(df_Q950,2,quantile,0.5),
  x = xADD
)

myplot <- ggplot(df_plot,aes(x = x))+
  geom_line(aes(y = median_low),col = "brown",
            linetype = "dotted",size = 1)+
  # geom_line(aes(y = median_up),col = "red",
  #           linetype = "dotted",size = 1)+
  # geom_line(aes(y = low_median), col = "brown",
  # linetype = "dotted",size = 1)+
  geom_line(aes(y = up_median), col = "brown",
            linetype = "dotted",size = 1)+
  geom_line(aes(y = Median),size = 1.5, col = "blue")

myplot1 <- myplot + 
  geom_point(data = DataList,
             aes(x = dose,y = RR, group = Study,col = Study,
                 shape = Study),
             size =2)+
  scale_shape_manual(values = 1:10)+
  geom_errorbar(data = DataList,
                aes(x = dose,ymin = RR.l,ymax = RR.u,
                    group = Study,col = Study))+
  scale_y_continuous(breaks = 1:15)+
  labs(x = "ADD(ug/kg/day)",
       y = "Relative Risk")+
  theme_classic(base_size = 25)+
  coord_cartesian(ylim = c(0,15),xlim = c(0,5))

ggsave(
  plot= myplot1,
  dev ="svg",
  filename = paste0(mywd,"Iterative_Summary_",Appro,
                    "_weighted_",level,".svg"),
  width = 16,height = 16
)
