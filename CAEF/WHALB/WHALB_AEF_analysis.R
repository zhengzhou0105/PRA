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
               "PBMA","PBMABB")
modelList <- c("Linear","Power","Expo5","Hill")

level <- "overarching"

iter <- 1e3
batch <- 200
batchsize <- 5

DSName <- "simu_ADD"
xADD <- seq(from = 0, to = 5, length.out = iter)

library("tidyverse")

## Functions--------
get_AEF_RR <- function(RR,pc){
  AEF <- pc*(1-1/RR)
  AEF <- ifelse(AEF <0,0,AEF)
  return(AEF)
}
get_RR_AEF <- function(AEF,pc){
  RR <- 1/(1-AEF/pc)
  RR <- ifelse(RR < 0,0,RR)
  return(RR)
}

fn_simu_iAsADD <- function(){
  # Exposure via drinking water
  # study 8 & 9 unit is ug/D not concentration
  waterConc <- df_water_simu %>% rowwise() %>% mutate(
    waterConc = rlnormTrunc(1,waterConc_meanlog,waterConc_sdlog,
                            min = waterConc_low,max = waterConc_up)
  )
  # conc * intake rate
  waterExpo_1 <- waterConc %>% filter(Index == 1) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.7636997,0.9686693,min = 0 )
  ) %>% select(Expo)
  waterExpo_2 <- waterConc %>% filter(Index == 2) %>% mutate(
    Expo = waterConc * rnormTrunc(1,2.28,1.02,min = 0)
  ) %>% select(Expo)
  waterExpo_3 <- waterConc %>% filter(Index == 3) %>% mutate(
    Expo = waterConc * runif(1,2,4)
  )%>% select(Expo)
  waterExpo_4 <- waterConc %>% filter(Index == 4) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.6,0.8,min = 0)
  )%>% select(Expo)
  waterExpo_5 <- waterConc %>% filter(Index == 5) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.2,1,min = 0)
  )%>% select(Expo)
  waterExpo_6 <- waterConc %>% filter(Index == 6) %>% mutate(
    Expo = waterConc * rnormTrunc(1,3,1,min = 0)
  )%>% select(Expo)
  waterExpo_7 <- waterConc %>% filter(Index == 7) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.9,1,min = 0)
  )%>% select(Expo)
  waterExpo_8 <- waterConc %>% filter(Index == 8) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_9 <- waterConc %>% filter(Index == 9) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_10 <- waterConc %>% filter(Index == 10) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.5,0.8,min = 0)
  ) %>% select(Expo)
  
  # Other Sources
  OAE_8 <- rlnorm(1,2.94,0.69)
  OAE_9 <- rlnorm(1,2.94,0.69)
  
  # ADD
  ADD_1 <- waterExpo_1 / 80 + 0.0319
  ADD_2 <- (waterExpo_2 + 10.7)/77
  ADD_3 <- waterExpo_3 / 65 + runif(3,0.079,1.04)
  ADD_4 <- waterExpo_4 / 72 + 0.0319
  ADD_5 <- waterExpo_5 / 80 + 0.0319
  ADD_6 <- (waterExpo_6 + 56.4)/60
  ADD_7 <- (waterExpo_7 + 38)/60
  ADD_8 <- (waterExpo_8 + rlnorm(4,2.94,0.69))/60
  ADD_9 <- (waterExpo_9 + rlnorm(4,2.94,0.69))/60
  ADD_10 <- waterExpo_10 / 80 + 0.0319
  
  ADD <- as.numeric(unlist(rbind(ADD_1,ADD_2,ADD_3,ADD_4,ADD_5,ADD_6,ADD_7,ADD_8,ADD_9,ADD_10)))
  ADDZ <- dnormalize_minmax(ADD)
  
  doseList <- list(
    dose = ADD,doseZ = ADDZ
  )
}

fn_simu_iAsExpo <- function(){
  # Exposure via drinking water
  # study 8 & 9 unit is ug/D not concentration
  waterConc <- df_water_simu %>% rowwise() %>% mutate(
    waterConc = rlnormTrunc(1,waterConc_meanlog,waterConc_sdlog,
                            min = waterConc_low,max = waterConc_up)
  )
  # conc * intake rate
  waterExpo_1 <- waterConc %>% filter(Index == 1) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.7636997,0.9686693,min = 0 )
  ) %>% select(Expo)
  waterExpo_2 <- waterConc %>% filter(Index == 2) %>% mutate(
    Expo = waterConc * rnormTrunc(1,2.28,1.02,min = 0)
  ) %>% select(Expo)
  waterExpo_3 <- waterConc %>% filter(Index == 3) %>% mutate(
    Expo = waterConc * runif(1,2,4)
  )%>% select(Expo)
  waterExpo_4 <- waterConc %>% filter(Index == 4) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.6,0.8,min = 0)
  )%>% select(Expo)
  waterExpo_5 <- waterConc %>% filter(Index == 5) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.2,1,min = 0)
  )%>% select(Expo)
  waterExpo_6 <- waterConc %>% filter(Index == 6) %>% mutate(
    Expo = waterConc * rnormTrunc(1,3,1,min = 0)
  )%>% select(Expo)
  waterExpo_7 <- waterConc %>% filter(Index == 7) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.9,1,min = 0)
  )%>% select(Expo)
  waterExpo_8 <- waterConc %>% filter(Index == 8) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_9 <- waterConc %>% filter(Index == 9) %>% rename(
    Expo = waterConc) %>% select(Expo)
  waterExpo_10 <- waterConc %>% filter(Index == 10) %>% mutate(
    Expo = waterConc * rnormTrunc(1,1.5,0.8,min = 0)
  ) %>% select(Expo)
  
  # Other Source
  OAE_8 <- rlnorm(1,2.94,0.69)
  OAE_9 <- rlnorm(1,2.94,0.69)
  
  ### Expo
  ADD_1 <- waterExpo_1 + 0.0319 * 80
  ADD_2 <- (waterExpo_2 + 10.7)
  ADD_3 <- waterExpo_3  + runif(3,0.079,1.04) * 65
  ADD_4 <- waterExpo_4  + 0.0319 * 72
  ADD_5 <- waterExpo_5  + 0.0319 * 80
  ADD_6 <- (waterExpo_6 + 56.4)
  ADD_7 <- (waterExpo_7 + 38)
  ADD_8 <- (waterExpo_8 + rlnorm(4,2.94,0.69))
  ADD_9 <- (waterExpo_9 + rlnorm(4,2.94,0.69))
  ADD_10 <- waterExpo_10  + 0.0319 * 80
  
  Expo <- as.numeric(unlist(rbind(ADD_1,ADD_2,ADD_3,ADD_4,ADD_5,ADD_6,ADD_7,ADD_8,ADD_9,ADD_10)))
  
  df_Expo_comp <- df_water_simu %>% select(Index,waterConc_low,waterConc_up)
  df_Expo_comp$waterConc_up[c(6,15,18,28,32,36)] <- c(14,0.75,13,102.2,107.6,65)
  df_Expo_comp <- df_Expo_comp %>% mutate(
    waterConc_med = (waterConc_up + waterConc_low) / 2,
    ExpoRatio = Expo / waterConc_med
  )
  
  doseList <- list(
    Expo = Expo,
    Ratio = df_Expo_comp$ExpoRatio
  )
}

## Comparing water and oral expo-------
## read dataList from files or run BHBMD_simu_ADD
df_raw <- read.csv(
  file = paste0(fileDir,"Metadata_bladder_iAs_censored.csv"))
df_water_simu <- read.csv(
  file = paste0(fileDir,"iAs_water_censored_simulation.csv"))

BW_vec <- c(80,80,80,80,80,80,
            77,77,77,
            65,65,65,
            72,72,72,
            80,80,80,
            60,60,60,
            60,60,60,
            60,60,60,60,
            60,60,60,60,
            80,80,80,80
            )
ADD_water <- df_Expo_comp$waterConc_med / BW_vec

Expo_list <- lapply(
  as.list(1:1000),function(x){
    return(fn_simu_iAsExpo())
  }
)


ADD_list <- lapply(
  as.list(1:1000),function(x){
    return(fn_simu_iAsADD()$dose)
  }
  
)
ADD_df <- do.call(rbind,ADD_list)
value = c(
    apply(ADD_df,2,median),
    apply(ADD_df,2,function(x) {quantile(x,0.05)}),
    apply(ADD_df,2,function(x) {quantile(x,0.95)}),
    ADD_water
)
X = rep(1:36,4)
type = c(
    rep("median",36),
    rep("Q5",36),
    rep("Q95",36),
    rep("raw",36)
)
ADD_err <- data.frame(Index = df_water_simu$Index,
                      X = as.character(1:36),
                      low = apply(ADD_df,2,function(x) {quantile(x,0.05)}),
                      up = apply(ADD_df,2,function(x) {quantile(x,0.95)}),
                      median = apply(ADD_df,2,median),
                      raw = ADD_water
                        )
myplot <- ggplot(ADD_err,aes(x = X))+
  facet_wrap(~Index,ncol = 5,scales = "free")+
  geom_point(aes(y = median))+
  geom_errorbar(aes(ymin = low, ymax = up))+
  geom_point(aes(y = raw),col = "blue")+
  labs(x = "Data Point Index", y = "ADD (ug/kg/day)",
       title = "All Oral Sources vs Drinking Water")+
  theme_classic(base_size = 20)

print(myplot)  

png("ADD oral vs water.png",width = 1200,height = 1200)
print(myplot)
dev.off()

## AEI based on CSF--------
BC_iAs <- rbeta(1e3,4.91,1.85)
Conc_iAs <- rlnorm(1e3,-2.87,0.94)
IRBW_rice <- rlnorm(1e3,0.14,1.42)
ADD_batch <- BC_iAs * Conc_iAs * IRBW_rice
ADD_sort <- sort(ADD_batch)
  
vec_CSF <- EnvStats::rnormTrunc(1e3,mean = 0.731e-3,sd = 0.4e-4 ,min = 0)

a_CUP <- 3.6e-5

df_T <- data.frame(
  x = ADD_sort
) %>% rowwise() %>% mutate(
  ADD_q = mean(x >= ADD_sort)
)
ls_AEI_T <- lapply(ADD_sort,function(x){
  return(x * vec_CSF)
})
mtx_AEI_T <- data.frame(ls_AEI_T)
df_T <- cbind(df_T,
  Median = apply(mtx_AEI_T,2,quantile,0.5),
  Q025 = apply(mtx_AEI_T,2,quantile,0.025),
  Q975 = apply(mtx_AEI_T,2,quantile,0.975)
) 

## AEI based on WHALB-----
df_fd_weighted <- read.csv("Summary_Iterative_RR_LPML_weighted.csv")

df_W <- df_fd_weighted %>% mutate(
  AEF_Median = get_AEF_RR(Median,pc =1),
  AEF_up = get_AEF_RR(Up,pc =1),
  AEF_low = get_AEF_RR(Low, pc = 1),
  AEI_Median = AEF_Median * a_CUP,
  AEI_up = AEF_up * a_CUP,
  AEI_low = AEF_low * a_CUP
) %>% rowwise() %>% mutate(
  ADD_q = mean(x >= ADD_batch)
)

## Plot AEI/AEF CSF vs WHALB----

  # traditional approach
plot <- ggplot(data = df_T,aes(x = ADD_q))+
  geom_line(aes(y = Median*1e5), col = "red",size = 1.5)+
  # geom_line(aes(y = Q025*1e5),col = "pink",
  #           linetype = "dotted",size = 1)+
  geom_line(aes(y = Q975*1e5),col = "pink",
            linetype = "dotted",size = 1)

plot <- plot +
  # WHALB approach
  geom_line(data = df_W,
    aes(x = ADD_q, y = AEI_Median*1e5),
            col = "blue",size = 1.5)+
  geom_line(data = df_W,
            aes(x = ADD_q, y = AEI_up *1e5),
            col = "green",linetype = "dotted",size = 1)+
  geom_hline(yintercept = 3.6,col ="purple",size = 2,
             linetype = "dashed")+
  coord_cartesian(xlim = c(0,1),ylim = c(0,5))+
  scale_y_continuous(
    breaks = c(1:5,3.6),
    sec.axis = sec_axis(~. /a_CUP/1e5,
                        name = "Attributable Excessive Fraction(%)")
  )+
  labs(y = "Attributable Excessive Incidence per 100,000 Population",
     # x = "iAs Exposure via Rice(ug/kg/day)",
     x = "Exposure Percentile(%)")+
  theme_classic(base_size = 21)

ggsave(
  plot = plot,
  filename = "Attributable Excessive Incidence between Traditional and WHALB Approaches.svg",
  device = "svg",
  width = 16,
  height = 16
)

# plot1 <- ggplot(df_W,aes(x = x))+
#   geom_histogram(aes(x = AEF_Median,y = ..density..),
#                  binwidth = 0.01,
#                  fill = "white",col = "black")+
#   geom_density(aes(x = AEF_Median,y = ..density..),
#                col = "red")+
#   coord_cartesian(xlim = c(0,1))+
#   theme_classic(base_size = 25)
# plot1
