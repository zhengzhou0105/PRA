# I119 PA analysis
# Zheng Zhou
# 11/11/2020
# Global settings--------
set.seed(47401)
setwd("C:/Users/Zheng Zhou/Documents/PHD work/Vanessa 2020 GA/Vanessa Kercher Lab/i119 Information")
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(reshape2)
library(readr)
library(tidyverse)
library(stringr)
library(readxl)


# clean FA20 data---------------
# load processed data with sentiment scores
FA20Pre <- read.csv("FA20Pre.csv")

# list of PA variables:
# VigPAdays	VigPAmin	ModPAdays	ModPAmin	WalkPAdays	WalkPAmin	StrengthPAdays	FlexPAdays	SitDay

FA20Pre$VigDay <- parse_number(FA20Pre$VigPAdays)
table(FA20Pre$VigDay)

FA20Pre$ModDay <- parse_number(FA20Pre$ModPAdays)
table(FA20Pre$ModDay)

FA20Pre$WalkDay <- parse_number(FA20Pre$WalkPAdays)
table(FA20Pre$WalkDay)

FA20Pre$FlexDay <- parse_number(FA20Pre$FlexPAdays)
table(FA20Pre$FlexDay)

# create an utility function to convert time into minutes
timeconvert <- function(x){
  time <- gsub(pattern = "\\D",replacement = "",x)
  if (str_length(as.character(time)) == 1){
    h <- time
    min <- 0
  } else if(str_length(as.character(time)) == 2){
    min <- time
    h <- 0
  } else {
    h <- str_sub(time,start = 1,end = 1)
    min <- str_sub(time,start = 2,end = 3)
  }
  minutes <- as.numeric(h) * 60 + as.numeric(min)
  return(minutes)
}

nFA20Pre <- nrow(FA20Pre)

FA20Pre$VigMin <- vector(length = nFA20Pre)
FA20Pre$ModMin <- vector(length = nFA20Pre)
FA20Pre$WalkMin <- vector(length = nFA20Pre)

# PA minutes
for(i in 1:nFA20Pre){
  FA20Pre$VigMin[i] <- timeconvert(FA20Pre$VigPAmin[i])
  FA20Pre$ModMin[i] <- timeconvert(FA20Pre$ModPAmin[i])
  FA20Pre$WalkMin[i] <- timeconvert(FA20Pre$WalkPAmin[i])
}

# PA MET-min based on IPAQ scoring
FA20Pre$VigMET <- FA20Pre$VigDay * FA20Pre$VigMin * 8
# FA20Pre$VigMET[is.na(FA20Pre$VigMET)] <- NULL 

FA20Pre$ModMET <- FA20Pre$ModDay * FA20Pre$ModMin * 4

FA20Pre$WalkMET <- FA20Pre$WalkDay * FA20Pre$WalkMin * 3.3


FA20Pre$TotalMET <- apply(FA20Pre[,131:133],1,sum,na.rm=TRUE)
FA20Pre$TotalMET[which(FA20Pre$TotalMET == 0)] <- NA

# identify moderately active individuals- moderate or walk at least five days /week
FA20Pre$ActiveMod <- ifelse((FA20Pre$VigDay >= 3 & FA20Pre$VigMin >= 20) | 
                              (FA20Pre$ModDay >= 5 & FA20Pre$ModMin >= 30) | (FA20Pre$WalkDay >= 5 & FA20Pre$WalkMin >= 30) |
                              ((FA20Pre$VigDay + FA20Pre$ModDay+ FA20Pre$WalkDay) >= 5) & FA20Pre$TotalMET >= 600,
                            1,0)

table(FA20Pre$ActiveMod)

FA20Pre$ActiveHigh <- ifelse( (FA20Pre$VigDay >=3 &  FA20Pre$VigMET >= 1500)|
                                (FA20Pre$VigDay + FA20Pre$ModDay+ FA20Pre$WalkDay) >= 7 | FA20Pre$TotalMET  >= 3000,
                              1,0)

table(FA20Pre$ActiveHigh)

Overlap <- which(FA20Pre$ActiveMod == 1 & FA20Pre$ActiveHigh == 1)
FA20Pre$ActiveMod[Overlap] <- 0

Modlow <- which(FA20Pre$ActiveMod == 0 & FA20Pre$ActiveHigh == 0)
FA20Pre$ActiveLow <- 0
FA20Pre$ActiveLow[Modlow] <- 1

FA20Pre$Active <- ifelse(FA20Pre$ActiveHigh == 1, "high",
       ifelse(FA20Pre$ActiveMod == 1, "moderate",
              "low"))

table(FA20Pre$Active)

FA20PreMoveHelp1 <- FA20Pre[,"Q70_1"]   # What are the top THREE things we can do to help you move more throughout  COVID?
FA20PreMoveHelp2 <- FA20Pre[,"Q70_2"]
FA20PreMoveHelp3 <- FA20Pre[,"Q70_4"]
FA20PreMoveHelp <- paste(FA20PreMoveHelp1,",",FA20PreMoveHelp2,",",FA20PreMoveHelp3)
FA20Pre$TopThreeForHelp <- FA20PreMoveHelp

# write.csv(FA20Pre,"I119 FA20 pre survey IPAQ scored.csv",row.names = F)



# clean SP20 pre data--------

# load data
SP20Pre <- read.csv("I119 20SP pre survey Qualtrics.csv")
head(SP20Pre$Name)
SP20Pre <- SP20Pre[-c(1:2),]

# list of PA variables:
# VigPAdays	VigPAmin	ModPAdays	ModPAmin	WalkPAdays	WalkPAmin	StrengthPAdays	FlexPAdays	SitDay

SP20Pre$VigDay <- parse_number(SP20Pre$VigPAdays)
table(SP20Pre$VigDay)

SP20Pre$ModDay <- parse_number(SP20Pre$ModPAdays)
table(SP20Pre$ModDay)

SP20Pre$WalkDay <- parse_number(SP20Pre$WalkPAdays)
table(SP20Pre$WalkDay)

SP20Pre$FlexDay <- parse_number(SP20Pre$FlexPAdays)
table(SP20Pre$FlexDay)

nSP20Pre <- nrow(SP20Pre)

SP20Pre$VigMin <- vector(length = nSP20Pre)
SP20Pre$ModMin <- vector(length = nSP20Pre)
SP20Pre$WalkMin <- vector(length = nSP20Pre)

# PA minutes
for(i in 1:nSP20Pre){
  SP20Pre$VigMin[i] <- timeconvert(SP20Pre$VigPAmin[i])
  SP20Pre$ModMin[i] <- timeconvert(SP20Pre$ModPAmin[i])
  SP20Pre$WalkMin[i] <- timeconvert(SP20Pre$WalkPAmin[i])
}

# PA MET-min based on IPAQ scoring
SP20Pre$VigMET <- SP20Pre$VigDay * SP20Pre$VigMin * 8

SP20Pre$ModMET <- SP20Pre$ModDay * SP20Pre$ModMin * 4

SP20Pre$WalkMET <- SP20Pre$WalkDay * SP20Pre$WalkMin * 3.3

which(colnames(SP20Pre) == "WalkMET")
SP20Pre$TotalMET <- apply(SP20Pre[,100:102],1,sum,na.rm=TRUE)
SP20Pre$TotalMET[which(SP20Pre$TotalMET == 0)] <- NA

# identify moderately active individuals- moderate or walk at least five days /week
SP20Pre$ActiveMod <- ifelse((SP20Pre$VigDay >= 3 & SP20Pre$VigMin >= 20) | 
                               (SP20Pre$ModDay >= 5 & SP20Pre$ModMin >= 30) | (SP20Pre$WalkDay >= 5 & SP20Pre$WalkMin >= 30) |
                               ((SP20Pre$VigDay + SP20Pre$ModDay+ SP20Pre$WalkDay) >= 5) & SP20Pre$TotalMET >= 600,
                             1,0)

table(SP20Pre$ActiveMod)

SP20Pre$ActiveHigh <- ifelse( (SP20Pre$VigDay >=3 &  SP20Pre$VigMET >= 1500)|
                                 (SP20Pre$VigDay + SP20Pre$ModDay+ SP20Pre$WalkDay) >= 7 | SP20Pre$TotalMET  >= 3000,
                               1,0)

table(SP20Pre$ActiveHigh)

Overlap <- which(SP20Pre$ActiveMod == 1 & SP20Pre$ActiveHigh == 1)
SP20Pre$ActiveMod[Overlap] <- 0

Modlow <- which(SP20Pre$ActiveMod == 0 & SP20Pre$ActiveHigh == 0)
SP20Pre$ActiveLow <- 0
SP20Pre$ActiveLow[Modlow] <- 1

SP20Pre$Active <- ifelse(SP20Pre$ActiveHigh == 1, "high",
                          ifelse(SP20Pre$ActiveMod == 1, "moderate",
                                 "low"))

table(SP20Pre$Active)


# write.csv(SP20Pre,"I119 SP20 pre survey IPAQ scored.csv",row.names = F)

# clean SP20 post data------

# load data
SP20Post <- read.csv("I119 20SP post survey qualtrics.csv")
SP20Post <- SP20Post[-c(1:2),]
colnames(SP20Post)[which(colnames(SP20Post)== "PostName")] <- "Name"

# list of PA variables:
# Post_VigPAdays	VigPAmin	ModPAdays	ModPAmin	WalkPAdays	WalkPAmin	StrengthPAdays	FlexPAdays	SitDay

SP20Post$VigDay <- parse_number(SP20Post$PostVigPAdays)
table(SP20Post$VigDay)

SP20Post$ModDay <- parse_number(SP20Post$PostModPAdays)
table(SP20Post$ModDay)

SP20Post$WalkDay <- parse_number(SP20Post$PostWalkPAdays)
table(SP20Post$WalkDay)

SP20Post$FlexDay <- parse_number(SP20Post$PostFlexPAdays)
table(SP20Post$FlexDay)

nSP20Post <- nrow(SP20Post)

SP20Post$VigMin <- vector(length = nSP20Post)
SP20Post$ModMin <- vector(length = nSP20Post)
SP20Post$WalkMin <- vector(length = nSP20Post)

# PA minutes
for(i in 1:nSP20Post){
  SP20Post$VigMin[i] <- timeconvert(SP20Post$PostVigPAmin[i])
  SP20Post$ModMin[i] <- timeconvert(SP20Post$PostModPAmin[i])
  SP20Post$WalkMin[i] <- timeconvert(SP20Post$PostWalkPAmin[i])
}

# PA MET-min based on IPAQ scoring
SP20Post$VigMET <- SP20Post$VigDay * SP20Post$VigMin * 8

SP20Post$ModMET <- SP20Post$ModDay * SP20Post$ModMin * 4

SP20Post$WalkMET <- SP20Post$WalkDay * SP20Post$WalkMin * 3.3

which(colnames(SP20Post) == "VigMET")
SP20Post$TotalMET <- apply(SP20Post[,73:75],1,sum,na.rm=TRUE)
SP20Post$TotalMET[which(SP20Post$TotalMET == 0)] <- NA

# identify moderately active individuals- moderate or walk at least five days /week
SP20Post$ActiveMod <- ifelse((SP20Post$VigDay >= 3 & SP20Post$VigMin >= 20) | 
                              (SP20Post$ModDay >= 5 & SP20Post$ModMin >= 30) | (SP20Post$WalkDay >= 5 & SP20Post$WalkMin >= 30) |
                              ((SP20Post$VigDay + SP20Post$ModDay+ SP20Post$WalkDay) >= 5) & SP20Post$TotalMET >= 600,
                            1,0)

table(SP20Post$ActiveMod)

SP20Post$ActiveHigh <- ifelse( (SP20Post$VigDay >=3 &  SP20Post$VigMET >= 1500)|
                                (SP20Post$VigDay + SP20Post$ModDay+ SP20Post$WalkDay) >= 7 | SP20Post$TotalMET  >= 3000,
                              1,0)

table(SP20Post$ActiveHigh)

Overlap <- which(SP20Post$ActiveMod == 1 & SP20Post$ActiveHigh == 1)
SP20Post$ActiveMod[Overlap] <- 0

Modlow <- which(SP20Post$ActiveMod == 0 & SP20Post$ActiveHigh == 0)
SP20Post$ActiveLow <- 0
SP20Post$ActiveLow[Modlow] <- 1

SP20Post$Active <- ifelse(SP20Post$ActiveHigh == 1, "high",
                         ifelse(SP20Post$ActiveMod == 1, "moderate",
                                "low"))

table(SP20Post$Active)


# write.csv(SP20Post,"I119 SP20 post survey IPAQ scored.csv",row.names = F)

# clean FA19 pre data------

# load data
FA19Pre <- read.csv("I119 19FA pre survey_MASTER Excel File_Text Responses.csv")
head(FA19Pre$Name)
FA19Pre <- FA19Pre[-1,]

# list of PA variables:
# VigPAdays	VigPAmin	ModPAdays	ModPAmin	WalkPAdays	WalkPAmin	StrengthPAdays	FlexPAdays	SitDay

FA19Pre$VigDay <- parse_number(FA19Pre$VigPAdays)
table(FA19Pre$VigDay)

FA19Pre$ModDay <- parse_number(FA19Pre$ModPAdays)
table(FA19Pre$ModDay)

FA19Pre$WalkDay <- parse_number(FA19Pre$WalkPAdays)
table(FA19Pre$WalkDay)

FA19Pre$FlexDay <- parse_number(FA19Pre$FlexPAdays)
table(FA19Pre$FlexDay)

nFA19Pre <- nrow(FA19Pre)

FA19Pre$VigMin <- vector(length = nFA19Pre)
FA19Pre$ModMin <- vector(length = nFA19Pre)
FA19Pre$WalkMin <- vector(length = nFA19Pre)

# PA minutes
for(i in 1:nFA19Pre){
  FA19Pre$VigMin[i] <- timeconvert(FA19Pre$VigPAmin[i])
  FA19Pre$ModMin[i] <- timeconvert(FA19Pre$ModPAmin[i])
  FA19Pre$WalkMin[i] <- timeconvert(FA19Pre$WalkPAmin[i])
}
summary(FA19Pre$VigMin)
summary(FA19Pre$ModMin)
summary(FA19Pre$WalkMin)

# PA MET-min based on IPAQ scoring
FA19Pre$VigMET <- FA19Pre$VigDay * FA19Pre$VigMin * 8

FA19Pre$ModMET <- FA19Pre$ModDay * FA19Pre$ModMin * 4

FA19Pre$WalkMET <- FA19Pre$WalkDay * FA19Pre$WalkMin * 3.3

which(colnames(FA19Pre) == "VigMET")
FA19Pre$TotalMET <- apply(FA19Pre[,114:116],1,sum,na.rm=TRUE)
FA19Pre$TotalMET[which(FA19Pre$TotalMET == 0)] <- NA

# identify moderately active individuals- moderate or walk at least five days /week
FA19Pre$ActiveMod <- ifelse((FA19Pre$VigDay >= 3 & FA19Pre$VigMin >= 20) | 
                               (FA19Pre$ModDay >= 5 & FA19Pre$ModMin >= 30) | (FA19Pre$WalkDay >= 5 & FA19Pre$WalkMin >= 30) |
                               ((FA19Pre$VigDay + FA19Pre$ModDay+ FA19Pre$WalkDay) >= 5) & FA19Pre$TotalMET >= 600,
                             1,0)

table(FA19Pre$ActiveMod)

FA19Pre$ActiveHigh <- ifelse( (FA19Pre$VigDay >=3 &  FA19Pre$VigMET >= 1500)|
                                 (FA19Pre$VigDay + FA19Pre$ModDay+ FA19Pre$WalkDay) >= 7 | FA19Pre$TotalMET  >= 3000,
                               1,0)

table(FA19Pre$ActiveHigh)

Overlap <- which(FA19Pre$ActiveMod == 1 & FA19Pre$ActiveHigh == 1)
FA19Pre$ActiveMod[Overlap] <- 0

Modlow <- which(FA19Pre$ActiveMod == 0 & FA19Pre$ActiveHigh == 0)
FA19Pre$ActiveLow <- 0
FA19Pre$ActiveLow[Modlow] <- 1

FA19Pre$Active <- ifelse(FA19Pre$ActiveHigh == 1, "high",
                          ifelse(FA19Pre$ActiveMod == 1, "moderate",
                                 "low"))

table(FA19Pre$Active)


# write.csv(FA19Pre,"I119 FA19 pre survey IPAQ scored.csv",row.names = F)

# clean FA19 post data-------

# load data
FA19Post <- read.csv("I119 19FA post survey Qualtrics.csv")
head(FA19Post$PostName)
FA19Post <- FA19Post[-c(1:3),]
colnames(FA19Post)[which(colnames(FA19Post)== "PostName")] <- "Name"

# list of PA variables:
# VigPAdays	VigPAmin	ModPAdays	ModPAmin	WalkPAdays	WalkPAmin	StrengthPAdays	FlexPAdays	SitDay

FA19Post$VigDay <- parse_number(FA19Post$VigPAdays)
table(FA19Post$VigDay)

FA19Post$ModDay <- parse_number(FA19Post$ModPAdays)
table(FA19Post$ModDay)

FA19Post$WalkDay <- parse_number(FA19Post$WalkPAdays)
table(FA19Post$WalkDay)

FA19Post$FlexDay <- parse_number(FA19Post$FlexPAdays)
table(FA19Post$FlexDay)

nFA19Post <- nrow(FA19Post)

FA19Post$VigMin <- vector(length = nFA19Post)
FA19Post$ModMin <- vector(length = nFA19Post)
FA19Post$WalkMin <- vector(length = nFA19Post)

# PA minutes
for(i in 1:nFA19Post){
  FA19Post$VigMin[i] <- timeconvert(FA19Post$VigPAmin[i])
  FA19Post$ModMin[i] <- timeconvert(FA19Post$ModPAmin[i])
  FA19Post$WalkMin[i] <- timeconvert(FA19Post$WalkPAmin[i])
}
summary(FA19Post$VigMin)
summary(FA19Post$ModMin)
summary(FA19Post$WalkMin)

# PA MET-min based on IPAQ scoring
FA19Post$VigMET <- FA19Post$VigDay * FA19Post$VigMin * 8

FA19Post$ModMET <- FA19Post$ModDay * FA19Post$ModMin * 4

FA19Post$WalkMET <- FA19Post$WalkDay * FA19Post$WalkMin * 3.3

which(colnames(FA19Post) == "VigMET")
FA19Post$TotalMET <- apply(FA19Post[,71:73],1,sum,na.rm=TRUE)
FA19Post$TotalMET[which(FA19Post$TotalMET == 0)] <- NA

# identify moderately active individuals- moderate or walk at least five days /week
FA19Post$ActiveMod <- ifelse((FA19Post$VigDay >= 3 & FA19Post$VigMin >= 20) | 
                              (FA19Post$ModDay >= 5 & FA19Post$ModMin >= 30) | (FA19Post$WalkDay >= 5 & FA19Post$WalkMin >= 30) |
                              ((FA19Post$VigDay + FA19Post$ModDay+ FA19Post$WalkDay) >= 5) & FA19Post$TotalMET >= 600,
                            1,0)

table(FA19Post$ActiveMod)

FA19Post$ActiveHigh <- ifelse( (FA19Post$VigDay >=3 &  FA19Post$VigMET >= 1500)|
                                (FA19Post$VigDay + FA19Post$ModDay+ FA19Post$WalkDay) >= 7 | FA19Post$TotalMET  >= 3000,
                              1,0)

table(FA19Post$ActiveHigh)

Overlap <- which(FA19Post$ActiveMod == 1 & FA19Post$ActiveHigh == 1)
FA19Post$ActiveMod[Overlap] <- 0

Modlow <- which(FA19Post$ActiveMod == 0 & FA19Post$ActiveHigh == 0)
FA19Post$ActiveLow <- 0
FA19Post$ActiveLow[Modlow] <- 1

FA19Post$Active <- ifelse(FA19Post$ActiveHigh == 1, "high",
                         ifelse(FA19Post$ActiveMod == 1, "moderate",
                                "low"))

table(FA19Post$Active)


# write.csv(FA19Post,"I119 FA19 post survey IPAQ scored.csv",row.names = F)

# Descriptive MET-min-----

# load data 
FA20Pre <- read.csv("I119 FA20 pre survey IPAQ scored.csv")
nFA20Pre <- nrow(FA20Pre)

SP20Pre <- read.csv("I119 SP20 pre survey IPAQ scored.csv")
nSP20Pre <- nrow(SP20Pre)

SP20Post <- read.csv("I119 SP20 post survey IPAQ scored.csv")
nSP20Post <- nrow(SP20Post)

FA19Pre <- read.csv("I119 FA19 pre survey IPAQ scored.csv")
nFA19Pre<- nrow(FA19Pre)

FA19Post <- read.csv("I119 FA19 post survey IPAQ scored.csv")
nFA19Post <- nrow(FA19Post)

  
# aggregated data                  
keep_PA <- c("Name",
             "VigDay","VigMin","VigMET",
             "ModDay","ModMin","ModMET",
             "WalkDay","WalkMin","WalkMET",
             "TotalMET","Active")
PA_df <- rbind(FA19Pre[,keep_PA],
               FA19Post[,keep_PA],
               SP20Pre[,keep_PA],
               SP20Post[,keep_PA],
               FA20Pre[,keep_PA])
PA_df <- data.frame(Term = (c(rep("2019FA Pre",nFA19Pre),
                                       rep("2019FA Post",nFA19Post),
                                       rep("2020SP Pre",nSP20Pre),
                                       rep("2020SP Post",nSP20Post),
                                       rep("2020FA Pre",nFA20Pre))),
                    PA_df)
PA_df$Term <- factor(PA_df$Term, levels = c("2019FA Pre","2019FA Post","2020SP Pre","2020SP Post","2020FA Pre"))


# PA_df <- data.frame(Term = c(rep("2020SP Pre",nSP20Pre),
#                              rep("2020SP Post",nSP20Post),
#                              rep("2020FA Pre",nFA20Pre),
#                              rep("2019FA Pre",nFA19Pre),
#                              rep("2019FA Post",nFA19Post)),
#                     VigDay = c(SP20Pre$VigDay, SP20Post$VigDay,FA20Pre$VigDay,FA19Pre$VigDay,FA19Post$VigDay),
#                     VigMin = c(SP20Pre$VigMin, SP20Post$VigMin,FA20Pre$VigMin,FA19Pre$VigMin,FA19Post$VigMin),
#                     VigMET = c(SP20Pre$VigMET, SP20Post$VigMET,FA20Pre$VigMET,FA19Pre$VigMET,FA19Post$VigMET),
#                     ModDay = c(SP20Pre$ModDay, SP20Post$ModDay,FA20Pre$ModDay),FA19Pre$ModDay,FA19Post$ModDay,
#                     ModMin = c(SP20Pre$ModMin, SP20Post$ModMin,FA20Pre$ModMin,FA19Pre$ModMin,FA19Post$ModMin),
#                     ModMET = c(SP20Pre$ModMET, SP20Post$ModMET,FA20Pre$ModMET,FA19Pre$ModMET,FA19Post$ModMET),
#                     WalkDay = c(SP20Pre$WalkDay, SP20Post$WalkDay,FA20Pre$WalkDay,FA19Pre$WalkDay,FA19Post$WalkDay),
#                     WalkMin = c(SP20Pre$WalkMin, SP20Post$WalkMin,FA20Pre$WalkMin,FA19Pre$WalkMin,FA19Post$WalkMin),
#                     WalkMET = c(SP20Pre$WalkMET, SP20Post$WalkMET,FA20Pre$WalkMET,FA19Pre$WalkMET,FA19Post$WalkMET),
#                     TotalMET = c(SP20Pre$TotalMET, SP20Post$TotalMET,FA20Pre$TotalMET,FA19Pre$TotalMET,FA19Post$TotalMET),
#                     Active = c(SP20Pre$Active, SP20Post$Active,FA20Pre$Active,FA19Pre$Active,FA19Post$Active),
#                     Name = c(SP20Pre$Name, SP20Post$Name,FA20Pre$Name,FA19Pre$Name,FA19Post$Name),
#                     Gender = c(SP20Pre$Gender, rep(NA,nSP20Post),FA20Pre$Gender,FA19Pre$Gender,rep(NA,nFA19Post)),
#                     Age = c(SP20Pre$Age, rep(NA,nSP20Post),FA20Pre$Age,FA19Pre$Age,rep(NA,nFA19Post)),
#                     School = c(SP20Pre$School, rep(NA,nSP20Post),FA20Pre$School,FA19Pre$School,rep(NA,nFA19Post)),
#                     Major = c(SP20Pre$Major, rep(NA,nSP20Post),FA20Pre$Major),FA19Pre$Major,rep(NA,nFA19Post))

# this is a sample of mostly highly active individuals
# which is shown as the total MET-mintues and ActiveHigh

tapply(X = PA_df$TotalMET, INDEX = PA_df$Term, FUN = function(x) c(N=length(x),summary(x)))

# tapply(X = PA_df$TotalMET, INDEX = PA_df$Term, hist)
# tapply(X = PA_df$TotalMET, INDEX = PA_df$Term, boxplot)


hist(FA20Pre$TotalMET)
boxplot(FA20Pre$TotalMET)
ecdf(FA20Pre$TotalMET)(600)  # where the WHO guideline is.

hist(SP20Pre$TotalMET)
boxplot(SP20Pre$TotalMET)
ecdf(SP20Pre$TotalMET)(600)  # where the WHO guideline is.

hist(SP20Post$TotalMET)
boxplot(SP20Post$TotalMET)
ecdf(SP20Post$TotalMET)(600)  # where the WHO guideline is.

base <- ggplot(data = PA_df,aes(y=TotalMET,by = Term))
base + geom_boxplot(aes(colour = Term))+
  geom_vline(xintercept = -0.075,colour = "blue")+
  geom_vline(xintercept = 0.225,colour="red", )
base+ stat_ecdf(aes(x = TotalMET, colour = Term),
                size = 1.2)

# show student names and PA by Total MET high to low
showlist <- c("Name",
              "VigDay","VigMin","VigMET",
              "ModDay","ModMin","ModMET",
              "WalkDay","WalkMin","WalkMET",
              "TotalMET","Active",
              "Major","School",
              "Age","Gender")
TotalMETrank <- order(FA20Pre$TotalMET,decreasing = T)
FA20Pre[TotalMETrank,which(colnames(FA20Pre) %in% showlist)] %>% head(20)


# School ---------

# FA20
aggregate(x = FA20Pre$TotalMET,
          by = list(FA20Pre$School),
          FUN= function(x) median(x))

FA20Pre$SchoolCombined <- ifelse(FA20Pre$School == "" | FA20Pre$School == "Undecided" |
                           FA20Pre$School == "Other", "Other",FA20Pre$School)
table(FA20Pre$SchoolCombined)
FA20Pre[which(FA20Pre$SchoolCombined == "Other"),"TotalMET"]

aggregate(x = FA20Pre$TotalMET,
          by = list(FA20Pre$SchoolCombined),
          FUN= function(x) median(x,na.rm = T))

# SPH vs non-SPH
FA20Pre$SPH <- ifelse(FA20Pre$SchoolCombined == "Public Health","yes","no")
table(FA20Pre$SPH)

t.test(TotalMET ~ SPH, data = FA20Pre)   
# 95%CI= (-508.9, 806.9), p = 0.6549, mu ~ 3200 MET
# which is about 400 mins/week of Vig, or 80 mins/day for 5days/week.

# FA19 
aggregate(x = FA19Pre$TotalMET,
          by = list(FA19Pre$School),
          FUN= function(x) median(x))

FA19Pre$SchoolCombined <- ifelse(FA19Pre$School == "" | FA19Pre$School == "Undecided" |
                                   FA19Pre$School == "Other", "Other",FA19Pre$School)
table(FA19Pre$SchoolCombined)
FA19Pre[which(FA19Pre$SchoolCombined == "Other"),"TotalMET"]

aggregate(x = FA19Pre$TotalMET,
          by = list(FA19Pre$SchoolCombined),
          FUN= function(x) median(x,na.rm = T))

FA19Pre$SPH <- ifelse(FA19Pre$SchoolCombined == "Public Health","yes","no")
table(FA19Pre$SPH)

t.test(TotalMET ~ SPH, data = FA19Pre)

# SP20 

aggregate(x = SP20Pre$TotalMET,
          by = list(SP20Pre$School),
          FUN= function(x) median(x))

SP20Pre$SchoolCombined <- ifelse(SP20Pre$School == "" | SP20Pre$School == "Undecided" |
                                   SP20Pre$School == "Other", "Other",SP20Pre$School)
table(SP20Pre$SchoolCombined)
SP20Pre[which(SP20Pre$SchoolCombined == "Other"),"TotalMET"]

aggregate(x = SP20Pre$TotalMET,
          by = list(SP20Pre$SchoolCombined),
          FUN= function(x) median(x,na.rm = T))

SP20Pre$SPH <- ifelse(SP20Pre$SchoolCombined == "Public Health","yes","no")
table(SP20Pre$SPH)

t.test(TotalMET ~ SPH, data = SP20Pre)

# Major--------------

# FA20
# exercise, kinesiology and others
table(FA20Pre$Major)
unique(FA20Pre$Major)

# detect exercise in major. 
Exer <- regex(c("exercise","exercise"),ignore_case = T)
MajorExer <- str_detect(FA20Pre$Major,Exer)
MajorExer[which(FA20Pre$Major == "Finance. However I am thinking about pursuing a double major in exercise science.")] <- F
summary(MajorExer)

# detect kinesiology major
Kine <- regex(c("kinesiology"),ignore_case = T)
MajorKine <- str_detect(FA20Pre$Major,Kine)
summary(MajorKine)

# detect fitness & wellness
FitWell <- regex(c("fitness","wellness"),ignore_case = T)
MajorFitWell <- str_detect(FA20Pre$Major,FitWell)
summary(MajorFitWell)

FA20Pre$MajorExerKine <- ifelse(MajorExer | MajorKine , "yes" ,"no")
table(FA20Pre$MajorExerKine)

t.test(TotalMET ~ MajorExerKine,
       data = FA20Pre)
# no significant difference between Exer/Kine major and others

# FA19
# exercise, kinesiology and others
table(FA19Pre$Major)
unique(FA19Pre$Major)

# detect exercise in major. 
Exer <- regex(c("exercise","exercise"),ignore_case = T)
MajorExer <- str_detect(FA19Pre$Major,Exer)
# MajorExer[which(FA19Pre$Major == "Finance. However I am thinking about pursuing a double major in exercise science.")] <- F
summary(MajorExer)

# detect kinesiology major
Kine <- regex(c("kinesiology"),ignore_case = T)
MajorKine <- str_detect(FA19Pre$Major,Kine)
summary(MajorKine)

# detect fitness & wellness
FitWell <- regex(c("fitness","wellness"),ignore_case = T)
MajorFitWell <- str_detect(FA19Pre$Major,FitWell)
summary(MajorFitWell)

FA19Pre$MajorExerKine <- ifelse(MajorExer | MajorKine , "yes" ,"no")
table(FA19Pre$MajorExerKine)

t.test(TotalMET ~ MajorExerKine,
       data = FA19Pre)

# SP20
# exercise, kinesiology and others
table(SP20Pre$Major)
unique(SP20Pre$Major)

# detect exercise in major. 
Exer <- regex(c("exercise","exercise"),ignore_case = T)
MajorExer <- str_detect(SP20Pre$Major,Exer)
# MajorExer[which(FA19Pre$Major == "Finance. However I am thinking about pursuing a double major in exercise science.")] <- F
summary(MajorExer)

# detect kinesiology major
Kine <- regex(c("kinesiology"),ignore_case = T)
MajorKine <- str_detect(SP20Pre$Major,Kine)
summary(MajorKine)

# detect fitness & wellness
FitWell <- regex(c("fitness","wellness"),ignore_case = T)
MajorFitWell <- str_detect(SP20Pre$Major,FitWell)
summary(MajorFitWell)

SP20Pre$MajorExerKine <- ifelse(MajorExer | MajorKine , "yes" ,"no")
table(SP20Pre$MajorExerKine)

t.test(TotalMET ~ MajorExerKine,
       data = SP20Pre)

# Major Visualization---
ggplot(data = FA20Pre,aes(x=as.factor(MajorExerKine),y = TotalMET))+
  geom_boxplot()+
  theme_classic(base_size = 20)+
  xlab("Major in Exercise Science or Kinesiology")+ylab("Total MET-minutes/ week")

ggplot(data = FA20Pre,aes(x = TotalMET, colour = MajorExerKine))+
  stat_ecdf(size = 1.2)+
  theme_classic(base_size = 20)+
  xlab("Total MET-minutes/ week")+
  ylab("Empirical Cumulative Density")+
  labs(colour = "Major in Exercise Science or Kinesiology")+
  theme(legend.position = "bottom")

# conclusion is, no statistical difference in MET between Exer/ Kine and other majors, except a few extreme cases. 
# Other factors?-------

# FA20
table(FA20Pre$Course)
table(FA20Pre$Gender)
FA20Pre$Gender <- as.factor(ifelse(FA20Pre$Gender == "Female","Female",
                                   ifelse(FA20Pre$Gender == "Male" , "Male" , "Others")))
anova(lm(TotalMET ~ Gender, data = FA20Pre))

# also want to examine the issue brought by Dani
SP20Pre$Gender <- factor(ifelse(SP20Pre$Gender == "Female","Female",
                                ifelse(FA20Pre$Gender == "Male" , "Male" , "Others")))
table(SP20Pre$Gender)
# write.csv(FA20Pre,"I119 FA20 pre survey PA analysis.csv", row.names = F)

# FA19
table(FA19Pre$Course)
table(FA19Pre$Gender)
FA19Pre$Gender <- as.factor(ifelse(FA19Pre$Gender == "Female","Female",
                                   ifelse(FA19Pre$Gender == "Male" , "Male" , "Others")))
anova(lm(TotalMET ~ Gender, data = FA19Pre))

# SP20
table(SP20Pre$Course)
table(SP20Pre$Gender)
SP20Pre$Gender <- as.factor(ifelse(SP20Pre$Gender == "Female","Female",
                                   ifelse(SP20Pre$Gender == "Male" , "Male" , "Others")))
anova(lm(TotalMET ~ Gender, data = SP20Pre))

# write.csv(FA20Pre,"I119 FA20 pre survey PA analysis.csv", row.names = F)
# Total PA with sentiment-----
summary(FA20Pre$MoveDiffSentiment)
hist(FA20Pre$MoveDiffSentiment)

# Total MET ~ sentiment after COVID
mod0 <- lm(TotalMET ~ MovePostSentiment + MajorExerKine+SPH + relevel(Gender,ref = "Others"),
           data = FA20Pre) 
summary(mod0)

# Total MET ~ sentiment change
mod1 <- lm(TotalMET ~ MoveDiffSentiment + MajorExerKine +SPH + relevel(Gender,ref = "Others"),
           data = FA20Pre) 
summary(mod1)

# Activity Specific PA with sentiment---------

# Vig PA
Vigmod <- lm(VigMET ~ MoveDiffSentiment + MajorExerKine+SPH + relevel(Gender,ref = "Others"),
             data = FA20Pre)
summary(Vigmod)

# Mod PA
Modmod <- lm(ModMET ~ MoveDiffSentiment + MajorExerKine+SPH + relevel(Gender,ref = "Others"),
             data = FA20Pre)
summary(Modmod)


# Walk PA
Walkmod <- lm(WalkMET ~ MoveDiffSentiment + MajorExerKine+SPH + relevel(Gender,ref = "Others"),
             data = FA20Pre)
summary(Walkmod)

# text mining validation---------

# validate the meaning of workout
# among students with negative sentiment diff

# load data
text <- FA20PreMoveHelp %>% VectorSource() %>% VCorpus()

## text transformation        transform the following punctuations into space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, toSpace, "\\|")

# Convert the text to lower case
text <- tm_map(text, content_transformer(tolower))
# Remove numbers
text <- tm_map(text, removeNumbers)
# Remove english common stopwords
text <- tm_map(text, removeWords, stopwords("english"))
# Remove defined stopwords
text <- tm_map(text, removeWords, c("give","make","can"))
# Remove punctuations
text <- tm_map(text, removePunctuation)
# Eliminate extra white spaces
text <- tm_map(text, stripWhitespace)

dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = T)
d <- data.frame(word = names(v), freq = v)

head(d,20)

workout <- regex(c("workout","workouts"),ignore_case = T)
IdenWorkout <- str_detect(workout,FA20PreMoveHelp)


# write.csv(FA20Pre,"I119 FA20 pre survey IPAQ scored.csv", row.names = F)
