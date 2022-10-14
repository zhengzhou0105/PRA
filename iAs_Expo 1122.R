## Arsenic Exposure ##

set.seed(12345)
## =====Crystal ball fitting==========
# Input Data #
input1<-read.csv("rice consumption data 1115.csv", header = TRUE)
input2<-read.csv("rice iAs sampling results 1115.csv", header = TRUE)
BW<-input1[,14]
IR.orig<-input1[,210]
C.orig<-input2[,12]

IR.no0<-IR.orig[IR.orig!=0]
min(IR.no0)
IR.adjust<-IR.orig
IR.adjust[IR.orig==0]<-min(IR.no0)/10
IRBW.adjust<-IR.adjust/BW

C.no0<-C.orig[C.orig!=0]
min(C.no0)
C.adjust<-C.orig
C.adjust[C.orig==0]<-min(C.no0)/2

meanlog.IR<-mean(log(IR.adjust))
sdlog.IR<-sd(log(IR.adjust))
meanlog.IR
sdlog.IR

meanlog.C<-mean(log(C.adjust))
sdlog.C<-sd(log(C.adjust))
meanlog.C
sdlog.C

meanlog.IRBW<-mean(log(IRBW.adjust))
sdlog.IRBW<-sd(log(IRBW.adjust))
meanlog.IRBW
sdlog.IRBW

iter<-10000
IR.sim<-rlnorm(iter,meanlog.IR,sdlog.IR)
IRBW.sim<-rlnorm(iter,meanlog.IRBW,sdlog.IRBW)
C.sim<-rlnorm(iter,meanlog.C,sdlog.C)
BC.sim<-rbeta(iter,2.91,1.52)

Intake.iAs.sim<-IR.sim*C.sim*BC.sim
mean(Intake.iAs.sim)
median(Intake.iAs.sim)
quantile(Intake.iAs.sim,0.025)
quantile(Intake.iAs.sim,0.975)

ADD.iAs.sim<-IRBW.sim*C.sim*BC.sim
mean(ADD.iAs.sim)
median(ADD.iAs.sim)
quantile(ADD.iAs.sim,0.025)
quantile(ADD.iAs.sim,0.975)

# ----------visualization--------
library(ggplot2)
# daily consumption
input1$IRadj <- IR.adjust
# by sex
aggregate(x= input1$IRadj,
          by= list(input1$sex),
          FUN=function(x) c(mean=mean(x), sd=sd(x), quantile(x,0.95)))
# by city
aggregate(x= input1$IRadj,
          by= list(input1$season),
          FUN=function(x) c(mean=mean(x), sd=sd(x), quantile(x,0.95)))
# by city and sex
aggregate(x= input1$IRadj,
          by= list(input1$sex, input1$city),
          FUN=function(x) c(mean=mean(x), sd=sd(x), quantile(x,0.95)))
aggregate(x= input1$IRadj,
          by= list(input1$city, input1$season),
          FUN=function(x) c(mean=mean(x), sd=sd(x), quantile(x,0.95)))
ggplot(input1, aes(x=season,y=IRadj, fill=city))+
  geom_boxplot()+
  scale_y_continuous(name="Daily Consumption of Rice and Rice Products(g dry weight/day",
                     limits = c(0,600),
                     breaks = seq(from=0,to=600,by=50))+
  facet_grid(.~sex)

# concentration
input2$B <- 1
for(i in 1: nrow(input2) ){
  if(input2$Type[i] == "brown"){
    input2$B[i] <- 1
  } else {
    input2$B[i] <- 0
  }
}


# daily exposure
perc <- seq(0,100, length.out = 10000)
yperc.exp <- quantile(Intake.iAs.sim,perc/100)
yperc.add <- quantile(ADD.iAs.sim,perc/100)
iAsplotting <- as.data.frame(cbind(perc,yperc.exp,yperc.add)) 

# daily exposure
ggplot(iAsplotting,aes(x=perc,y=yperc.exp))+
  geom_line(size=1.4)+
  scale_y_continuous(limits = c(0,80),breaks = seq(from=0,to=80,by=5),
                     name = "Daily exposure of iAs via rice intake(ug/day)")+
  scale_x_continuous(name="Percentile",breaks = seq(from=0,to=100,by=5))+
  geom_hline(yintercept = 1.1,lty="dotted",color="purple",size=1.2)+
  geom_hline(yintercept = 2.8, lty="dashed",color="blue",size=1.2)+
  geom_hline(yintercept = 10.5, lty="longdash",color="green",size=1.2)+
  geom_hline(yintercept = 24.54, lty="twodash" , color="orange",size=1.2)

# ADD


ggplot(iAsplotting,aes(x=perc,y=yperc.add))+
  geom_line(size=1.4)+
  scale_y_continuous(limits = c(0,1.5),breaks = seq(from=0,to=1.5,by=0.1),
                     name = "ADD(ug/kg/day)")+
  scale_x_continuous(name="Percentile",breaks = seq(from=0,to=100,by=5))+
  geom_hline(yintercept = 0.179,lty="dotted",color="green",size=1.2)+
  geom_hline(yintercept = 0.3, lty="dashed",color="red",size=1.2)+
  geom_hline(yintercept = 31.9/1000, lty="longdash",color="blue",size=1.2)+
  geom_hline(yintercept = 0.374, lty= "twodash" , color="orange" , size=1.2)

# ==========alternative fitting=======\
library(fitdistrplus)
descdist(IR.adjust)
descdist(C.adjust)
descdist(IRBW.adjust)

# IR
plot(fitdist(IR.adjust,"weibull",method = "mle"))
summary(fitdist(IR.adjust,"weibull",method = "mle"))
shape.IR <- 1.176065
scale.IR <- 118.863245

# IRBW
plot(fitdist(IRBW.adjust,"weibull",method = "mle"))
summary(fitdist(IRBW.adjust,"weibull",method = "mle"))
shape.IRBW <- 1.166919
scale.IRBW <- 1.952847

# Conc
plot(fitdist(C.adjust,"weibull",method = "mle"))
summary(fitdist(C.adjust,"weibull",method = "mle"))
shape.c <- 1.40670376
scale.c <- 0.08372015

# using mle algorithm, weibull model is the most appropriate based on the lowest AIC
# for IR, IRBW, C

# simulation
iter<-10000
IR.weib.sim <- rweibull(iter,shape.IR,scale.IR)
IRBW.weib.sim <- rweibull(iter,shape.IRBW,scale.IRBW)
C.weib.sim<-rweibull(iter,shape.c,scale.c)

Intake.iAs.weib.sim<-IR.weib.sim*C.weib.sim*BC.sim
mean(Intake.iAs.weib.sim)
median(Intake.iAs.weib.sim)
quantile(Intake.iAs.weib.sim,c(0.025,0.975))


ADD.iAs.weib.sim<-IRBW.weib.sim*C.weib.sim*BC.sim
mean(ADD.iAs.weib.sim)
median(ADD.iAs.weib.sim)
quantile(ADD.iAs.weib.sim,c(0.025,0.975))

# ----------visualization--------
perc <- seq(0,100, length.out = 10000)
yperc.exp.weib <- quantile(Intake.iAs.weib.sim,perc/100)
yperc.add.weib <- quantile(ADD.iAs.weib.sim,perc/100)
iAsplotting <- as.data.frame(cbind(perc,yperc.exp,yperc.add,yperc.exp.weib,yperc.add.weib)) 

# Histogram
# daily exposure
ggplot(iAsplotting,aes(yperc.exp))+
  geom_histogram(breaks=seq(from=0,to=300,by=5),stat = "density")+
  geom_histogram(aes(yperc.exp.weib),breaks=seq(from=0,to=300,by=5),stat = "density",color="red")

# cumulative density function plot
# daily exposure
ggplot(iAsplotting,aes(x=perc,y=yperc.exp))+
  geom_line(size=1.4)+
  scale_y_continuous(limits = c(0,80),breaks = seq(from=0,to=80,by=5),
                     name = "Daily exposure of iAs via rice intake(ug/day)")+
  scale_x_continuous(name="Percentile",breaks = seq(from=0,to=100,by=5))+
  geom_line(aes(x=perc,y=yperc.exp.weib),size=1.4,color="red")
  # geom_hline(yintercept = 1.1,lty="dotted",color="purple",size=1.2)+
  # geom_hline(yintercept = 2.8, lty="dashed",color="blue",size=1.2)+
  # geom_hline(yintercept = 10.5, lty="longdash",color="green",size=1.2)+
  # geom_hline(yintercept = 24.54, lty="twodash" , color="orange",size=1.2)+

# ADD
ggplot(iAsplotting,aes(x=perc,y=yperc.add))+
  geom_line(size=1.4)+
  scale_y_continuous(limits = c(0,1.5),breaks = seq(from=0,to=1.5,by=0.1),
                     name = "ADD(ug/kg/day)")+
  scale_x_continuous(name="Percentile",breaks = seq(from=0,to=100,by=5))+
  geom_line(aes(x=perc,y=yperc.add.weib),size=1.4,color="red")
  # geom_hline(yintercept = 0.179,lty="dotted",color="green",size=1.2)+
  # geom_hline(yintercept = 0.3, lty="dashed",color="red",size=1.2)+
  # geom_hline(yintercept = 31.9/1000, lty="longdash",color="blue",size=1.2)+
  # geom_hline(yintercept = 0.374, lty= "twodash" , color="orange" , size=1.2)