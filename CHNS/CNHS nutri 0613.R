# set working directory
# setwd("C:/Users/bks01/Box Sync/Temp/Kan's China rice As project/CHANES")

# data processing
# CHNS food data
# CHNS.food<-read.csv(file = "D:/Box Sync/Temp/Kan's China rice As project/CHANES/CHANESfood/food.csv")

# rice foodcode
# ricecode<-c(12001,12101,12102,12103,12104,12105,12106,12107,12108,12201,12202,12203,12204,12205,12206,12207,12208,
#            12209,12210,12211,12212,12213,12214,12215,12301,12302,12303,12304,12305,12306,12401,12402,12403,12404,
#            12405,12406,12408,12409,12410)

# CHNS rice data based on rice foodcode
# CHNS.rice<-subset(CHNS.food,FOODCODE %in% ricecode)
# write.csv(CHNS.rice,file = "D:/Box Sync/Temp/Kan's China rice As project/CHANES/CHANESfood/rice_97_11.csv")
# ===========================================================================
# original data:rice v0.csv is the data file identifed by the food code above
CHNS.rice<-read.csv(file = "rice v0.csv") # read v0: original data
# ------------------------------------------------------------
# create v1 without NA
CHNS.rice1<-CHNS.rice[!is.na(CHNS.rice$IR),] ## drop NA observations
# merge IR by ID
# IDlist <- unique(CHNS.rice1$hhid)
# for (i in 1:length(IDlist)){
#   temp <- subset(CHNS.rice1,CHNS.rice1$hhid == IDlist[i])
# }

# calculate age by test year
CHNS.rice1$age <- CHNS.rice1$WAVE - CHNS.rice1$birthyear
# write to csv to add agegroup
write.csv(CHNS.rice1,"rice v1.csv" , row.names = F)
# ===========================================================================
# add age group by hand in excel to get v2
# load edited data: v2 with age group
CHNS.rice2 <- read.csv("rice v2.csv")
# ------------------------------------------------------------
# adult data only
CHNS.rice.adult <- subset(CHNS.rice2, CHNS.rice2$age >= 18)
# ------------------------------------------------------------
# create v3 include adult since 2004
CHNS.rice3 <- subset(CHNS.rice.adult, CHNS.rice.adult$WAVE > 2000)
write.csv(CHNS.rice3,"adult since 2004.csv")
# ------------------------------------------------------------
# create v4 include adult & urban since 2004
CHNS.rice4 <- subset(CHNS.rice3,CHNS.rice3$T2 == "u")
write.csv(CHNS.rice4,"urban adults since 2004.csv")
# ===========================================================================
# data analysis
# all by wave
aggregate(x=CHNS.rice3$IR,by=list(year=CHNS.rice3$WAVE),
          FUN=function(x) c(n=length(x),mean=mean(x),sd=sd(x),quantile(x,c(0.05,0.50,0.95))))
# ------------------------------------------------------------
# urban adults by wave
ByWave <- aggregate(x=CHNS.rice4$IR,by=list(year=CHNS.rice4$WAVE),
          FUN=function(x) c(n=length(x),mean=round(mean(x),1),sd=round(sd(x),1),quantile(x,c(0.05,0.50,0.95))))
write.csv(ByWave,"CHNS urban adult since 2004 by wave.csv")
# urban adults by wave and sex
ByWaveSex <- aggregate(x=CHNS.rice4$IR,
          by=list(year=CHNS.rice4$WAVE,
                  sex=CHNS.rice4$GENDER),
          FUN=function(x) c(n=length(x),mean=round(mean(x),1),sd=round(sd(x),1),quantile(x,c(0.05,0.50,0.95))))
write.csv(ByWaveSex,"CHNS urban adult since 2004 by wave sex .csv")
# urban adult by wave,sex and age group
ByWaveSexAge <- aggregate(x=CHNS.rice4$IR,
          by=list(year=CHNS.rice4$WAVE,
                  sex=CHNS.rice4$GENDER,
                  Age=CHNS.rice4$AgeGrp),
          FUN=function(x) c(n=length(x),mean=round(mean(x),1),sd=round(sd(x),1),quantile(x,c(0.05,0.50,0.95))))
write.csv(ByWaveSexAge,"CHNS urban adult since 2004 by wave sex and age group.csv")
# ------------------------------------------------------------
# use boxplot to display all and urban data in one graph
library(ggplot2)
# create a dataset for side-byside boxplot
CHNS.rice.plot <- CHNS.rice3
# CHNS.rice.plot$T2 <- "Overall"
# CHNS.rice.plot <- rbind(CHNS.rice.plot,CHNS.rice4)
CHNS.rice.plot$WAVE <- factor(CHNS.rice.plot$WAVE)
boxplot1 <- ggplot(CHNS.rice.plot,aes(x=WAVE,y=IR,fill=T2)) + #overall and urban sidebyside
  geom_boxplot()+
  scale_y_continuous(limits = c(0,450), # adjust y axis range and name
                     name = "Log Daily Rice Consumption(g dry weight/day)")+
  scale_x_discrete(name = "Yeal of Panel")+
  scale_fill_discrete(name="Population Residency",labels=c("Rural","Urban"))
boxplot1
summary(lm(IR~WAVE+AgeGrp,data=CHNS.rice.plot))
# ------------------------------------------------------------
# export
SumBySexandYear <- aggregate(x=CHNS.rice4$IR,by=list(year=CHNS.rice4$WAVE,
                                  sex=CHNS.rice4$GENDER ),
          FUN=function(x) c(n=length(x),mean=mean(x),sd=sd(x),quantile(x,c(0.05,0.50,0.95))))
write.csv(SumBySexandYear, "Sum By Sex and Year.csv")
