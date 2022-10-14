# Analysis of I119 2020 FA pre and post data
# Author: Zheng Zhou
# Date: 12/26/2020

# settings--------

# setwd("C:/Users/bks01/OneDrive - Indiana University/Documents/PHD work/Vanessa 2020 GA/Vanessa Kercher Lab/i119 Information")
library(ggplot2)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyverse)
set.seed(47405)

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

# input---------

FA20Pre <- read.csv("I119 FA20 pre survey IPAQ scored.csv")                 # 20FA pre data from previous work

FA20Fitness <- read.csv("I119 20FA fitness assessment complete.csv")        # 20FA fitness assessment pre and post

FA20Post <- read.csv("I119 20FA post-survey.csv")                           # 20FA post data


# clean uncompleted responses--------

# List of issues:
# duplicate from the same subject
# incomplete response
# unnamed and untraceable responses
# pre and post do not match

which(FA20Pre$Progress != 100)                  # incompleted in pre
length(which(FA20Pre$Progress != 100))          # 14 uncompleted responses
FA20Pre$Progress[which(FA20Pre$Progress != 100)]  # value of progress


# Incomplete responses, could students come back later and make another take?

# among which # 267 and 268 never complete the survey. At 73% and 63% completion. Others are unnamed
# These two are kept. Then pre has 274-(14-2) = 262 valid data points.
FA20Pre <- FA20Pre[-c(2,3,13,53,54,141,145,173,174,266,273,274),]  # drop other incomplete responses

which(FA20Post$Progress != 100)                  # incompleted in post
length(which(FA20Post$Progress != 100))          # 10 uncompleted responses
FA20Post$Progress[which(FA20Post$Progress != 100)]  # value of progress

# name correction. Inconsistent name reported from one student: Ayana Karner
FA20Post$Name[which(FA20Post$Name == "Ayana")] <- "Ayana Karner"

# among which # 264 and 266 never complete the survey. At 2% and 86% completion. Others are unnamed
# # 264 is dropped for low completion. # 266 is kept.
# Post has 269-(10-1)=260 valid data points.
FA20Post <- FA20Post[-c(56,66,77,197,217,257,261,262,264),]   # drop other incomplete responses


# converge case of all names
FA20Pre$Name <- str_to_title(FA20Pre$Name)
FA20Post$Name <- str_to_title(FA20Post$Name)

# # output complete response
# write.csv(FA20Pre,"I119 20FA pre completed.csv",row.names = F)
# write.csv(FA20Pre,"I119 20FA pre completed.csv",row.names = F)

# match and merge----

#  match by student name:  FA20Pre.Name = FA20Post.Name

FA20Full <- full_join(x = FA20Pre,               # full join keeps all data from both datasets but mark unmatched as NA
                      y = FA20Post,
                      by = "Name",
                      suffix = c(".pre",".post"))

which(colnames(FA20Full) == "VigDay")
which(colnames(FA20Full) == "TopThreeForHelp")
colnames(FA20Full)[124:142] <- paste0(colnames(FA20Full)[124:142],".pre")

# write.csv(FA20Full,"I119 20FA Complete.csv",row.names = F)

# Valid data----
# # pre = 262, # post = 260, # pre | post = 308, so # pre & post = 216
NoPre <- is.na(FA20Full$StartDate.pre)                # No pre response
NoPost <- is.na(FA20Full$StartDate.post)              # No post response
Valid <-  !(NoPre | NoPost)                           # compliment to (NoPre OR NoPost)
FA20Valid <- FA20Full[Valid,]

# write.csv(FA20Valid, "I119 20FA full valid.csv",row.names = F)

# Physical Activity-----

# pre survey PA is analyzed already. with suffix .pre

# post survey PA

FA20Valid$VigDay.post <- parse_number(FA20Valid$VigPAdays.post)
table(FA20Valid$VigDay.post)

FA20Valid$ModDay.post <- parse_number(FA20Valid$ModPAdays.post)
table(FA20Valid$ModDay.post)

FA20Valid$WalkDay.post <- parse_number(FA20Valid$WalkPAdays.post)
table(FA20Valid$WalkDay.post)

FA20Valid$FlexDay.post <- parse_number(FA20Valid$FlexPAdays.post)
table(FA20Valid$FlexDay.post)

# create vector to store PA mins values
nFA20 <- nrow(FA20Valid)

FA20Valid$VigMin.post <- vector(length = nFA20)
FA20Valid$ModMin.post <- vector(length = nFA20)
FA20Valid$WalkMin.post <- vector(length = nFA20)

# PA minutes
for(i in 1:nFA20){
  FA20Valid$VigMin.post[i] <- timeconvert(FA20Valid$VigPAmin.post[i])
  FA20Valid$ModMin.post[i] <- timeconvert(FA20Valid$ModPAmin.post[i])
  FA20Valid$WalkMin.post[i] <- timeconvert(FA20Valid$WalkPAmin.post[i])
}

table(FA20Valid$VigMin.post)
table(FA20Valid$ModMin.post)
table(FA20Valid$WalkMin.post)

# PA MET-min based on IPAQ protocol

FA20Valid <- FA20Valid %>% 
  mutate(
    VigMET.post = VigDay.post * VigMin.post * 8,
    ModMET.post = ModDay.post * ModMin.post * 4,
    WalkMET.post = WalkDay.post * WalkMin.post * 3.3
    )
FA20Valid <- FA20Valid %>%
  replace_na(list(VigMET.post = 0,
                  ModMET.post = 0,
                  WalkMET.post = 0)
             )
summary(FA20Valid$VigMET.post)
summary(FA20Valid$ModMET.post)
summary(FA20Valid$WalkMET.post)

# Total MET/wk

FA20Valid <- FA20Valid %>%
  mutate(TotalMET.post = VigMET.post + ModMET.post + WalkMET.post
         )
FA20Valid$TotalMET.post[which(FA20Valid$TotalMET.post == 0)] <- NA

table(FA20Valid$TotalMET.post)

# Active status categorization

FA20Valid <- FA20Valid %>%
  mutate(ActiveMod.post = ifelse(
    (VigDay.post >= 3 & VigMin.post >=20) |
           (ModDay.post >= 5 & ModMin.post >= 30) |
            (VigDay.post + ModDay.post + WalkDay.post) >=5 &
             TotalMET.post >= 600,
    yes = 1, no = 0)
  )
table(FA20Valid$ActiveMod.post)

FA20Valid <- FA20Valid %>%
  mutate(
    ActiveHigh.post = ifelse(
      (VigDay.post >= 3 & VigMET.post >= 1500) |
        VigDay.post + ModDay.post+ WalkDay.post >= 7 | 
        TotalMET.post >= 3000,
      yes = 1, no = 0
    )
  )
table(FA20Valid$ActiveHigh.post)

Overlap <- which(FA20Valid$ActiveMod.post == 1 & FA20Valid$ActiveHigh.post == 1)
FA20Valid$ActiveMod.post[Overlap] <- 0

Modlow <- which(FA20Valid$ActiveMod.post == 0 & FA20Valid$ActiveHigh.post == 0)
FA20Valid$ActiveLow.post <- 0
FA20Valid$ActiveLow.post[Modlow] <- 1

FA20Valid$Active.post <- ifelse(FA20Valid$ActiveHigh.post == 1, "high",
                         ifelse(FA20Valid$ActiveMod.post == 1, "moderate",
                                "low"))

table(FA20Valid$Active.pre)
table(FA20Valid$Active.post)

# clean demographic info-------

# school
FA20Valid$SchoolCombined <- ifelse(FA20Valid$School == "" | FA20Valid$School == "Undecided" |
                                     FA20Valid$School == "Other", "Other",FA20Valid$School)

# SPH vs non-SPH
FA20Valid$SPH <- ifelse(FA20Valid$SchoolCombined == "Public Health","yes","no")
table(FA20Valid$SPH)

# Major
table(FA20Valid$Major)  # exercise, kinesiology and others
unique(FA20Valid$Major)

Exer <- regex(c("exercise","exercise"),ignore_case = T)   # detect exercise in major.
MajorExer <- str_detect(FA20Valid$Major,Exer)
MajorExer[which(FA20Valid$Major == "Finance. However I am thinking about pursuing a double major in exercise science.")] <- F
summary(MajorExer)

Kine <- regex(c("kinesiology"),ignore_case = T)  # detect kinesiology major
MajorKine <- str_detect(FA20Valid$Major,Kine)
summary(MajorKine)

FitWell <- regex(c("fitness","wellness"),ignore_case = T)  # detect fitness & wellness
MajorFitWell <- str_detect(FA20Valid$Major,FitWell)
summary(MajorFitWell)

FA20Valid$MajorExerKine <- ifelse(MajorExer | MajorKine , "yes" ,"no")
table(FA20Valid$MajorExerKine)


# Gender
table(FA20Valid$Gender)
sum(is.na(FA20Valid$Gender))
FA20Valid$Gender <- as.factor(ifelse(FA20Valid$Gender == "Female","Female",
                                   ifelse(FA20Valid$Gender == "Male" , "Male" , "Others")))

# Age
table(FA20Valid$Age)
sum(is.na(FA20Valid$Age))
which(is.na(FA20Valid$Age))
FA20Valid$Name[which(is.na(FA20Valid$Age))]
FA20Valid$Progress.pre[which(is.na(FA20Valid$Age))]

# Class
table(FA20Valid$ClassStat)
sum(is.na(FA20Valid$ClassStat))
which(FA20Valid$ClassStat == "")            # the same student who didn't complete the survey
FA20Valid$ClassStat[213] <- NA             


# PA analysis-----

# post PA and pre-post comparison

summary(FA20Valid$TotalMET.post)
summary(FA20Valid$TotalMET.pre)

plot(ecdf(FA20Valid$TotalMET.post),col="red",
     main = "", ylab = "ECDF")
lines(ecdf(FA20Valid$TotalMET.pre),col="blue")
abline(h = c(0,1),v=4300)

boxplot(FA20Valid$TotalMET.pre,FA20Valid$TotalMET.post,
        names = c("Pre","Post"))
t.test(x = log(FA20Valid$TotalMET.pre), y = log(FA20Valid$TotalMET.post),paired = T)
boxplot(log(FA20Valid$TotalMET.pre),log(FA20Valid$TotalMET.post),
        names = c("Pre","Post"))

FA20Valid <- FA20Valid %>% mutate(
  TotalMET.diff = TotalMET.post- TotalMET.pre
)
summary(FA20Valid$TotalMET.diff)
plot(density(FA20Valid$TotalMET.diff))
qqnorm(FA20Valid$TotalMET.diff)
qqline(FA20Valid$TotalMET.diff)


# by gender
anova(lm(TotalMET.diff ~ Gender, data = FA20Valid))
t.test(TotalMET.diff ~ factor(Gender,levels = c("Male","Female")), data = subset(FA20Valid, select = Gender != "Other"))

# by School
t.test(TotalMET.diff ~ SPH, data = FA20Valid)

# by Major
t.test(TotalMET.diff ~ MajorExerKine, data = FA20Valid)
summary(lm(TotalMET.diff ~ Major, data = FA20Valid))

# by session
summary(aov(TotalMET.pre ~ Course.post, data = FA20Valid))
summary(aov(TotalMET.post ~ Course.pre, data = FA20Valid))

# by class 
summary(aov(TotalMET.pre ~ ClassStat, data = FA20Valid))
summary(aov(TotalMET.post ~ ClassStat, data = FA20Valid))
summary(aov(TotalMET.diff ~ ClassStat, data = FA20Valid))

# PHQ9-----

# named as PHQ9 in pre and PHQ2 in post survey

# anhedonia: not at all = 0, several days = 3, more than half days = 4, nearly every day = 6
table(FA20Valid$PHQ2_1)
table(FA20Valid$PHQ9_1)

FA20Valid <- FA20Valid %>%
  mutate(
      PHQ2.1.pre = ifelse(PHQ9_1 ==  regex("Not at\nall") , yes = 0,
                          ifelse(PHQ9_1 ==  regex("Several\ndays") , yes = 1,
                                 ifelse(PHQ9_1 ==  regex("More than\nhalf days") , yes = 2, no=3)))
           )
FA20Valid <- FA20Valid %>%
  mutate(
    PHQ2.1.post = ifelse(PHQ2_1 ==  regex("Not at\nall") , yes = 0,
                        ifelse(PHQ2_1 ==  regex("Several\ndays") , yes = 1,
                               ifelse(PHQ2_1 ==  regex("More than\nhalf days") , yes = 2, no=3)))
  )

# depression mood: not at all = 0, several days = 3, more than half days = 4, nearly every day = 6
table(FA20Valid$PHQ2_2)
table(FA20Valid$PHQ9_2)

FA20Valid <- FA20Valid %>%
  mutate(
    PHQ2.2.pre = ifelse(PHQ9_2 ==  regex("Not at\nall") , yes = 0,
                        ifelse(PHQ9_2 ==  regex("Several\ndays") , yes = 1,
                               ifelse(PHQ9_2 ==  regex("More than\nhalf days") , yes = 2, no=3)))
  )
FA20Valid <- FA20Valid %>%
  mutate(
    PHQ2.2.post = ifelse(PHQ2_2 ==  regex("Not at\nall") , yes = 0,
                         ifelse(PHQ2_2 ==  regex("Several\ndays") , yes = 1,
                                ifelse(PHQ2_2 ==  regex("More than\nhalf days") , yes = 2, no=3)))
  )

# score 3 is the cut off point
# calculate proportion of students beyond the cut off point

FA20Valid <- FA20Valid %>% 
  mutate(
    PHQ2.pre = PHQ2.1.pre + PHQ2.2.pre,
    PHQ2.post = PHQ2.1.post + PHQ2.2.post,
    PHQ2Dep.pre = ifelse(PHQ2.pre <=3, yes = 0, no = 1),   # 0 = not depressed, 1 = depressed
    PHQ2Dep.post = ifelse(PHQ2.post <=3, yes = 0, no = 1)
  )

table(FA20Valid$PHQ2Dep.pre)
table(FA20Valid$PHQ2Dep.post)
plot(density(FA20Valid$PHQ2Dep.pre))
plot(density(FA20Valid$PHQ2Dep.post))

t.test(FA20Valid$PHQ2Dep.pre,FA20Valid$PHQ2Dep.post,
       paired = T)

# GAD-----

# GAD is GAD_1 to _7 in pre and post
# Over the last two weeks, how often have you been bothered by the following problems?
# 0 = None, 1 = 1-6 days, 2 = 7 days or more, 3 = Nearly\neveryday

FA20Valid <- FA20Valid %>%
  mutate(
    GAD1.pre = ifelse(GAD_1.pre ==  regex("None") , yes = 0,
                        ifelse(GAD_1.pre ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_1.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD2.pre = ifelse(GAD_2.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_2.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_2.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD3.pre = ifelse(GAD_3.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_3.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_3.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD4.pre = ifelse(GAD_4.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_4.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_4.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD5.pre = ifelse(GAD_5.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_5.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_5.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD6.pre = ifelse(GAD_6.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_6.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_6.pre ==  regex("7 days or more") , yes = 2, no=3))),
    GAD7.pre = ifelse(GAD_7.pre ==  regex("None") , yes = 0,
                       ifelse(GAD_7.pre ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_7.pre ==  regex("7 days or more") , yes = 2, no=3)))
    
  )

FA20Valid <- FA20Valid %>%
  mutate(
    GAD1.post = ifelse(GAD_1.post ==  regex("None") , yes = 0,
                       ifelse(GAD_1.post ==  regex("1-6 days") , yes = 1,
                              ifelse(GAD_1.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD2.post = ifelse(GAD_2.post ==  regex("None") , yes = 0,
                        ifelse(GAD_2.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_2.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD3.post = ifelse(GAD_3.post ==  regex("None") , yes = 0,
                        ifelse(GAD_3.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_3.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD4.post = ifelse(GAD_4.post ==  regex("None") , yes = 0,
                        ifelse(GAD_4.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_4.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD5.post = ifelse(GAD_5.post ==  regex("None") , yes = 0,
                        ifelse(GAD_5.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_5.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD6.post = ifelse(GAD_6.post ==  regex("None") , yes = 0,
                        ifelse(GAD_6.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_6.post ==  regex("7 days or more") , yes = 2, no=3))),
    GAD7.post = ifelse(GAD_7.post ==  regex("None") , yes = 0,
                        ifelse(GAD_7.post ==  regex("1-6 days") , yes = 1,
                               ifelse(GAD_7.post ==  regex("7 days or more") , yes = 2, no=3)))
    
  )

FA20Valid <- FA20Valid %>% mutate(
  GAD.total.pre = GAD1.pre + GAD2.pre + GAD3.pre + GAD4.pre+ GAD5.pre + GAD6.pre+ GAD7.pre,  # total GAD score
  GAD.total.post = GAD1.post + GAD2.post+ GAD3.post+GAD4.post+GAD5.post+GAD6.post+GAD7.post,
  GAD.pre = ifelse(GAD.total.pre <= 4, yes = "minimal",                         # category by total score
        no = ifelse(GAD.total.pre <= 9, "mild",
                    no=ifelse(GAD.total.pre <= 14,"moderate","severe"))),
  GAD.post = ifelse(GAD.total.post <= 4, yes = "minimal",
                    no = ifelse(GAD.total.post <= 9, "mild",
                                no=ifelse(GAD.total.post <= 14,"moderate","severe")))
)

table(factor(FA20Valid$GAD.pre,levels = c("minimal","mild","moderate","severe")))
table(factor(FA20Valid$GAD.post,levels = c("minimal","mild","moderate","severe")))

mcnemar.test(x = FA20Valid$GAD.pre, y = FA20Valid$GAD.post)

# MHSC-----
# MHSC as:
# pre survey: list(MHSC1_1	MHSC1_2	MHSC1_3	MHSC1_4	MHSC1_5 
#   Q70_1.1	Q70_2.1	Q70_3	Q70_4.1	Q70_5	Q71_1	Q71_2	Q71_3	Q71_4)
# post survey: MHSC-SF 14 = list(MHSC1_EmWB	MHSC2_EmWB	MHSC3_EmWB	MHSC4_SocWB	MHSC5_SocWB	MHSC6_SocWB	MHSC7_SocWB	
#   MHSC8_SocWB	MHSC9_PsyWB	MHSC10_PsyWB	MHSC11_PsyWB	MHSC12_PsyWB	MHSC13_PsyWB	MHSC14_PsyWB)

FA20Valid <- FA20Valid %>% mutate(
  MHCS.1.pre =   ifelse(
    MHSC1_1 == "Never", yes = 0,
    no = ifelse(
      MHSC1_1 == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_1 == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_1 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_1 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),

  MHCS.2.pre =   ifelse(
    MHSC1_2 == "Never", yes = 0,
    no = ifelse(
      MHSC1_2 == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_2 == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_2 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_2 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.3.pre =   ifelse(
    MHSC1_3 == "Never", yes = 0,
    no = ifelse(
      MHSC1_3 == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_3 == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_3 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_3 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.4.pre =   ifelse(
    MHSC1_4 == "Never", yes = 0,
    no = ifelse(
      MHSC1_4 == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_4 == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_4 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_4 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.5.pre =   ifelse(
    MHSC1_5 == "Never", yes = 0,
    no = ifelse(
      MHSC1_5 == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_5 == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_5 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_5 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.6.pre = ifelse(
    Q70_1.1 == "Never", yes = 0,
    no = ifelse(
      Q70_1.1 == "Once or Twice", yes = 1,
      no = ifelse(
        Q70_1.1 == "About once a week" , yes = 2,
        no = ifelse(
          Q70_1.1 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q70_1.1 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.7.pre = ifelse(
    Q70_2.1 == "Never", yes = 0,
    no = ifelse(
      Q70_2.1 == "Once or Twice", yes = 1,
      no = ifelse(
        Q70_2.1 == "About once a week" , yes = 2,
        no = ifelse(
          Q70_2.1 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q70_2.1 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.8.pre = ifelse(
    Q70_3 == "Never", yes = 0,
    no = ifelse(
      Q70_3 == "Once or Twice", yes = 1,
      no = ifelse(
        Q70_3 == "About once a week" , yes = 2,
        no = ifelse(
          Q70_3 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q70_3 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.9.pre = ifelse(
    Q70_4.1 == "Never", yes = 0,
    no = ifelse(
      Q70_4.1 == "Once or Twice", yes = 1,
      no = ifelse(
        Q70_4.1 == "About once a week" , yes = 2,
        no = ifelse(
          Q70_4.1 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q70_4.1 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.10.pre = ifelse(
    Q70_5 == "Never", yes = 0,
    no = ifelse(
      Q70_5 == "Once or Twice", yes = 1,
      no = ifelse(
        Q70_5 == "About once a week" , yes = 2,
        no = ifelse(
          Q70_5 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q70_5 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.11.pre = ifelse(
    Q71_1 == "Never", yes = 0,
    no = ifelse(
      Q71_1 == "Once or Twice", yes = 1,
      no = ifelse(
        Q71_1 == "About once a week" , yes = 2,
        no = ifelse(
          Q71_1 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q71_1 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.12.pre = ifelse(
    Q71_2 == "Never", yes = 0,
    no = ifelse(
      Q71_2 == "Once or Twice", yes = 1,
      no = ifelse(
        Q71_2 == "About once a week" , yes = 2,
        no = ifelse(
          Q71_2 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q71_2 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.13.pre = ifelse(
    Q71_3 == "Never", yes = 0,
    no = ifelse(
      Q71_3 == "Once or Twice", yes = 1,
      no = ifelse(
        Q71_3 == "About once a week" , yes = 2,
        no = ifelse(
          Q71_3 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q71_3 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.14.pre = ifelse(
    Q71_4 == "Never", yes = 0,
    no = ifelse(
      Q71_4 == "Once or Twice", yes = 1,
      no = ifelse(
        Q71_4 == "About once a week" , yes = 2,
        no = ifelse(
          Q71_4 == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            Q71_4 == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  )
)

FA20Valid <- FA20Valid %>% mutate(
  MHCS.1.post = ifelse(
    MHSC1_EmWB == "Never", yes = 0,
    no = ifelse(
      MHSC1_EmWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC1_EmWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC1_EmWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC1_EmWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.2.post = ifelse(
    MHSC2_EmWB == "Never", yes = 0,
    no = ifelse(
      MHSC2_EmWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC2_EmWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC2_EmWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC2_EmWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.3.post = ifelse(
    MHSC3_EmWB == "Never", yes = 0,
    no = ifelse(
      MHSC3_EmWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC3_EmWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC3_EmWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC3_EmWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.4.post = ifelse(
    MHSC4_SocWB == "Never", yes = 0,
    no = ifelse(
      MHSC4_SocWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC4_SocWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC4_SocWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC4_SocWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.5.post = ifelse(
    MHSC5_SocWB == "Never", yes = 0,
    no = ifelse(
      MHSC5_SocWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC5_SocWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC5_SocWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC5_SocWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.6.post = ifelse(
    MHSC6_SocWB == "Never", yes = 0,
    no = ifelse(
      MHSC6_SocWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC6_SocWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC6_SocWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC6_SocWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.7.post = ifelse(
    MHSC7_SocWB == "Never", yes = 0,
    no = ifelse(
      MHSC7_SocWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC7_SocWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC7_SocWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC7_SocWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.8.post = ifelse(
    MHSC8_SocWB == "Never", yes = 0,
    no = ifelse(
      MHSC8_SocWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC8_SocWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC8_SocWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC8_SocWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.9.post = ifelse(
    MHSC9_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC9_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC9_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC9_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC9_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.10.post = ifelse(
    MHSC10_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC10_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC10_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC10_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC10_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.11.post = ifelse(
    MHSC11_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC11_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC11_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC11_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC11_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.12.post = ifelse(
    MHSC12_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC12_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC12_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC12_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC12_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.13.post = ifelse(
    MHSC13_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC13_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC13_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC13_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC13_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  ),
  MHCS.14.post = ifelse(
    MHSC14_PsyWB == "Never", yes = 0,
    no = ifelse(
      MHSC14_PsyWB == "Once or Twice", yes = 1,
      no = ifelse(
        MHSC14_PsyWB == "About once a week" , yes = 2,
        no = ifelse(
          MHSC14_PsyWB == "2 or 3 times a week" , yes = 3,
          no = ifelse(
            MHSC14_PsyWB == "Almost Every Day" , yes = 4, no = 5
          )
        )
      )
    )
  )
)

FA20Valid <- FA20Valid %>% mutate(
  MHCS.0.pre = "",
  
  MHCS.0.post = ""
)

for(i in 1:nFA20){
  FA20Valid[i,"MHCS.0.pre"] <- if (
    sum(c(FA20Valid[i,"MHCS.1.pre"]>=4,FA20Valid[i,"MHCS.2.pre"]>=4,FA20Valid[i,"MHCS.3.pre"]>=4)) >= 1 &
    sum(c(FA20Valid[i,"MHCS.4.pre"]>=4,FA20Valid[i,"MHCS.5.pre"]>=4,FA20Valid[i,"MHCS.6.pre"]>=4,
          FA20Valid[i,"MHCS.7.pre"]>=4,FA20Valid[i,"MHCS.8.pre"]>=4,
          FA20Valid[i,"MHCS.9.pre"]>=4,FA20Valid[i,"MHCS.10.pre"]>=4,FA20Valid[i,"MHCS.11.pre"]>=4,
          FA20Valid[i,"MHCS.12.pre"]>=4,FA20Valid[i,"MHCS.13.pre"]>=4,FA20Valid[i,"MHCS.14.pre"]>=4)) >=6
  ) {"flourishing"
    } else if (
    sum(c(FA20Valid[i,"MHCS.1.pre"]<=1,FA20Valid[i,"MHCS.2.pre"]<=1,FA20Valid[i,"MHCS.3.pre"]<=1)) >= 1 &
    sum(c(FA20Valid[i,"MHCS.4.pre"]<=1,FA20Valid[i,"MHCS.5.pre"]<=1,FA20Valid[i,"MHCS.6.pre"]<=1,
          FA20Valid[i,"MHCS.7.pre"]<=1,FA20Valid[i,"MHCS.8.pre"]<=1,
          FA20Valid[i,"MHCS.9.pre"]<=1,FA20Valid[i,"MHCS.10.pre"]<=1,FA20Valid[i,"MHCS.11.pre"]<=1,
          FA20Valid[i,"MHCS.12.pre"]<=1,FA20Valid[i,"MHCS.13.pre"]<=1,FA20Valid[i,"MHCS.14.pre"]<=1)) >=6
    ){"languishing"
      } else {"moderately mentally healthy"}
}

table(FA20Valid$MHCS.0.pre)

for(i in 1:nFA20){
  FA20Valid[i,"MHCS.0.post"] <- if (
    sum(c(FA20Valid[i,"MHCS.1.post"]>=4,FA20Valid[i,"MHCS.2.post"]>=4,FA20Valid[i,"MHCS.3.post"]>=4)) >= 1 &
    sum(c(FA20Valid[i,"MHCS.4.post"]>=4,FA20Valid[i,"MHCS.5.post"]>=4,FA20Valid[i,"MHCS.6.post"]>=4,
          FA20Valid[i,"MHCS.7.post"]>=4,FA20Valid[i,"MHCS.8.post"]>=4,FA20Valid[i,"MHCS.9.post"]>=4,
          FA20Valid[i,"MHCS.10.post"]>=4,FA20Valid[i,"MHCS.11.post"]>=4,
          FA20Valid[i,"MHCS.12.post"]>=4,FA20Valid[i,"MHCS.13.post"]>=4,FA20Valid[i,"MHCS.14.post"]>=4)) >=6
  ) {"flourishing"
  } else if (
    sum(c(FA20Valid[i,"MHCS.1.post"]<=1,FA20Valid[i,"MHCS.2.post"]<=1,FA20Valid[i,"MHCS.3.post"]<=1)) >= 1 &
    sum(c(FA20Valid[i,"MHCS.4.post"]<=1,FA20Valid[i,"MHCS.5.post"]<=1,FA20Valid[i,"MHCS.6.post"]<=1,
          FA20Valid[i,"MHCS.7.post"]<=1,FA20Valid[i,"MHCS.8.post"]<=1,
          FA20Valid[i,"MHCS.9.post"]<=1,FA20Valid[i,"MHCS.10.post"]<=1,FA20Valid[i,"MHCS.11.post"]<=1,
          FA20Valid[i,"MHCS.12.post"]<=1,FA20Valid[i,"MHCS.13.post"]<=1,FA20Valid[i,"MHCS.14.post"]<=1)) >=6
  ){"languishing"
  } else {"moderately mentally healthy"}
}

table(FA20Valid$MHCS.0.post)

mcnemar.test(FA20Valid$MHCS.0.pre,FA20Valid$MHCS.0.post)

# Readiness----
# coded in pre as ReadyConf_NPS_GROUP.pre	ReadyConf.pre	ReadyImp_NPS_GROUP.pre	ReadyImp.pre	Readiness.pre	
# coded in post as ReadyConf_NPS_GROUP.post	ReadyConf.post	ReadyImp_NPS_GROUP.post	ReadyImp.post	Readiness.post
# examine confidence and importance level

FA20Valid <- FA20Valid %>% mutate(
  ReadyConf.diff = ReadyConf.post - ReadyConf.pre,                     # difference by original values
  ReadyImp.diff = ReadyImp.post-ReadyImp.pre,
  ReadyConf.pre1 = ifelse(ReadyConf.pre <= 6, yes= "no greater than 6", no = ReadyConf.pre),          # combine lower levels: <=6
  ReadyConf.post1 = ifelse(ReadyConf.post <= 6, yes= "no greater than 6", no = ReadyConf.post),
  ReadyImp.pre1 = ifelse(ReadyImp.pre <= 6, yes= "no greater than 6", no = ReadyImp.pre),
  ReadyImp.post1 = ifelse(ReadyImp.post <= 6, yes= "no greater than 6", no = ReadyImp.post)
)

table(FA20Valid$ReadyConf.pre)
table(FA20Valid$ReadyConf.post)

table(FA20Valid$ReadyConf.pre1)
table(FA20Valid$ReadyConf.post1)

table(FA20Valid$ReadyConf.diff)

qqnorm(FA20Valid$ReadyConf.diff)
qqline(FA20Valid$ReadyConf.diff)

plot(density(FA20Valid$ReadyConf.diff,na.rm = T))

t.test(FA20Valid$ReadyConf.pre,FA20Valid$ReadyConf.post,paired = T)
mcnemar.test(FA20Valid$ReadyConf.pre1,FA20Valid$ReadyConf.post1)

# FA20Valid$ReadyImp.pre <- factor(FA20Valid$ReadyImp.pre,levels = 1:10)
# FA20Valid$ReadyImp.post <- factor(FA20Valid$ReadyImp.post,levels = 1:10)
table(FA20Valid$ReadyImp.pre)                  # original values
table(FA20Valid$ReadyImp.post)

table(FA20Valid$ReadyImp.pre1)               # combined values
table(FA20Valid$ReadyImp.post1)

table(FA20Valid$ReadyImp.diff)

qqnorm(FA20Valid$ReadyImp.diff)
qqline(FA20Valid$ReadyImp.diff)

plot(density(FA20Valid$ReadyImp.diff,na.rm = T))

t.test(FA20Valid$ReadyImp.pre,FA20Valid$ReadyImp.post,paired = T)
mcnemar.test(FA20Valid$ReadyImp.pre1,FA20Valid$ReadyImp.post1)

# PA vs Readiness------
# idea is to treat readiness & importance as proxy of motivation
# first thing is to see correlation between PA pre and post and readiness & importance

# pre: MET vs imp
cor(FA20Valid$TotalMET.pre, FA20Valid$ReadyImp.pre,                    # spearman's rank correlation
    method = "spearman",use = "complete")
summary(aov(TotalMET.pre ~ ReadyImp.pre1, data = FA20Valid))

PAbase <- ggplot(FA20Valid)                                              # visualization
PAbase + geom_point(aes(x=ReadyImp.pre,y=TotalMET.pre))+
  geom_smooth(aes(x =ReadyImp.pre , y= TotalMET.pre),method = "glm")

# pre: MET vs conf
cor(FA20Valid$TotalMET.pre, FA20Valid$ReadyConf.pre,                    # spearman's rank correlation
    method = "spearman",use = "complete")
summary(aov(TotalMET.pre ~ ReadyConf.pre1, data = FA20Valid))
                                             # visualization
PAbase + geom_point(aes(x=ReadyConf.pre,y=TotalMET.pre))+
  geom_smooth(aes(x =ReadyConf.pre , y= TotalMET.pre),method = "glm")

# regression: pre MET ~ everything

mod.PA.pre <- lm(TotalMET.pre ~ Gender + MajorExerKine + SPH + ReadyConf.pre + ReadyImp.pre+
                   ClassStat, data = FA20Valid)          

# mod.PA.pre <- lm(TotalMET.pre ~ Gender + MajorExerKine + SPH + ReadyConf.pre + ReadyImp.pre+
#                    Course.pre+ClassStat, data = FA20Valid)           # course ref = F 9:10-10:50 Lanie & Dani
summary(mod.PA.pre)

mod1.PA.pre <- lm(TotalMET.pre ~ Gender + MajorExerKine + SPH + ReadyConf.pre1 + ReadyImp.pre1+
                    ClassStat, data = FA20Valid)          # course ref = F 9:10-10:50 Lanie & Dani
# mod1.PA.pre <- lm(TotalMET.pre ~ Gender + MajorExerKine + SPH + ReadyConf.pre1 + ReadyImp.pre1+
#                     Course.pre+ClassStat, data = FA20Valid)          # course ref = F 9:10-10:50 Lanie & Dani
summary(mod1.PA.pre)

# same analysis for post

# post: MET vs imp
cor(FA20Valid$TotalMET.post, FA20Valid$ReadyImp.post,                    # spearman's rank correlation
    method = "spearman",use = "complete")
summary(aov(TotalMET.post ~ ReadyImp.post1, data = FA20Valid))

PAbase <- ggplot(FA20Valid)                                              # visualization
PAbase + geom_point(aes(x=ReadyImp.post,y=TotalMET.post))+
  geom_smooth(aes(x =ReadyImp.post , y= TotalMET.post),method = "glm")

# post: MET vs conf
cor(FA20Valid$TotalMET.post, FA20Valid$ReadyConf.post,                    # spearman's rank correlation
    method = "spearman",use = "complete")
summary(aov(TotalMET.post ~ ReadyConf.post1, data = FA20Valid))
                                              # visualization
PAbase + geom_point(aes(x=ReadyConf.post,y=TotalMET.post))+
  geom_smooth(aes(x =ReadyConf.post , y= TotalMET.post),method = "glm")

# regression: post MET ~ everything

mod.PA.post <- lm(TotalMET.post ~ Gender + MajorExerKine + SPH + ReadyConf.post + ReadyImp.post+
                   ClassStat, data = FA20Valid)          

# mod.PA.post <- lm(TotalMET.post ~ Gender + MajorExerKine + SPH + ReadyConf.post + ReadyImp.post+
#                    Course.post+ClassStat, data = FA20Valid)           # course ref = F 9:10-10:50 Lanie & Dani
summary(mod.PA.post)

mod1.PA.post <- lm(TotalMET.post ~ Gender + MajorExerKine + SPH + ReadyConf.post1 + ReadyImp.post1+
                    ClassStat, data = FA20Valid)          # course ref = F 9:10-10:50 Lanie & Dani
# mod1.PA.post <- lm(TotalMET.post ~ Gender + MajorExerKine + SPH + ReadyConf.post1 + ReadyImp.post1+
#                     Course.post+ClassStat, data = FA20Valid)          # course ref = F 9:10-10:50 Lanie & Dani
summary(mod1.PA.post)

# change in MET vs change in confidence

mod.PA.diff <- lm(TotalMET.diff ~ Gender + MajorExerKine + SPH + ReadyConf.diff +
                    ReadyImp.diff + ClassStat , data = FA20Valid)
summary(mod.PA.diff)