# -----settings-----
library(dplyr)

# mannually check activity hours per day for abnormal values

# -----input------


SP2021Pre <- read.csv("SP2021 pre wellness assignment 1.csv")
SP2021Pre <- slice(SP2021Pre,-1)         # remove the first row(second row in excel)

SP2021Post <- read.csv("SP2021 post wellness assignment 13.csv")
SP2021Post <- slice(SP2021Post,-1)       # remove the first row(second row in excel)

# merge 
SP2021 <- left_join(SP2021Pre,SP2021Post,
                     by = c("Name" = "Q165"),
                     suffix = c(".pre",".post")) %>% rename(
                       Gender = Q185,
                       Age = Q186,
                       ClassStat = Q187,
                       Major = Q189,
                       SchoolCombined = Q188
                       
                     )

nSP2021 <- nrow(SP2021)

# -------demographics-------

# sex
table(SP2021[,"Gender"])

# school
table(SP2021[,"SchoolCombined"])

# class
table(SP2021[,"ClassStat"])

# age
summary(as.numeric((SP2021$Age)))
plot(ecdf(SP2021$Age),
     main = "empirical distribution of age",
     xlab = "age",
     ylab = "percentile",
     lwd = 5)

# -------physical activity

# extract days in " xxx D "
library(readr)
SP2021 <- SP2021 %>% mutate(VigDay.pre = parse_number(VigPAdays.pre),
                 ModDay.pre = parse_number(ModPAdays.pre),
                 WalkDay.pre = parse_number(WalkPAdays.pre),
                 VigDay.post = parse_number(VigPAdays.post),
                 ModDay.post = parse_number(ModPAdays.post),
                 WalkDay.post = parse_number(WalkPAdays.post)
                 )

# display
table(SP2021$VigDay.pre)
table(SP2021$VigDay.post)
table(SP2021$ModDay.pre)
table(SP2021$ModDay.post)
table(SP2021$WalkDay.pre)
table(SP2021$WalkDay.post)

# PA minutes

# calculate minutes
SP2021 <- SP2021 %>% mutate(
  
  VigMin.pre = as.numeric(VigPAmin_1.pre) * 60 + as.numeric(VigPAmin_33.pre),
  ModMin.pre = as.numeric(ModPAmin_1.pre) * 60 + as.numeric(ModPAmin_30.pre),
  WalkMin.pre = as.numeric(WalkPAmin_29.pre) * 60 + as.numeric(WalkPAmin_36.pre),
  
  VigMin.post = as.numeric(VigPAmin_1.post) * 60 + as.numeric(VigPAmin_33.post),
  ModMin.post = as.numeric(ModPAmin_1.pre) * 60 + as.numeric(ModPAmin_30.post),
  WalkMin.post = as.numeric(WalkPAmin_29.post) * 60 + as.numeric(WalkPAmin_36.post)
)

# summary
apply(SP2021[,c("VigMin.pre","ModMin.pre","WalkMin.pre",
                "VigMin.post","ModMin.post","WalkMin.post")],
      MARGIN = 2,summary)


# METs/wk by PA
SP2021 <- SP2021 %>% 
  mutate(
    VigMET.pre = VigDay.pre * VigMin.pre * 8,
    ModMET.pre = ModDay.pre * ModMin.pre * 4,
    WalkMET.pre = WalkDay.pre * WalkMin.pre * 3.3
  )
SP2021 <- SP2021 %>%
  tidyr::replace_na(list(VigMET.pre = 0,
                         ModMET.pre = 0,
                         WalkMET.pre = 0)
  )
summary(SP2021$VigMET.pre)
summary(SP2021$ModMET.pre)
summary(SP2021$WalkMET.pre)

SP2021 <- SP2021 %>% 
  mutate(
    VigMET.post = VigDay.post * VigMin.post * 8,
    ModMET.post = ModDay.post * ModMin.post * 4,
    WalkMET.post = WalkDay.post * WalkMin.post * 3.3
  )
SP2021 <- SP2021 %>%
  tidyr::replace_na(list(VigMET.post = 0,
                         ModMET.post = 0,
                         WalkMET.post = 0)
  )
summary(SP2021$VigMET.post)
summary(SP2021$ModMET.post)
summary(SP2021$WalkMET.post)

# Total MET/wk
SP2021 <- SP2021 %>% mutate(
  TotalMET.pre = VigMET.pre + ModMET.pre + WalkMET.pre,
  TotalMET.post = VigMET.post + ModMET.post + WalkMET.post
  )
SP2021$TotalMET.pre[which(SP2021$TotalMET.pre == 0)] <- NA
SP2021$TotalMET.post[which(SP2021$TotalMET.post == 0)] <- NA


SP2021 <- SP2021 %>% mutate(
  TotalMET.diff = TotalMET.post - TotalMET.pre
  )


# description
summary(SP2021$TotalMET.pre)
plot(ecdf(SP2021$TotalMET.pre),
     main = "empirical distribution",
     xlab = "Total METs/wk",ylab = "percentile")

summary(SP2021$TotalMET.post)
plot(ecdf(SP2021$TotalMET.post),
     main = "empirical distribution",
     xlab = "Total METs/wk",ylab = "percentile")

# difference in log scale
boxplot(log(SP2021$TotalMET.pre),log(SP2021$TotalMET.post),
        names = c("Pre","Post"),main ="at log scale")
t.test(x = log(SP2021$TotalMET.pre), y = log(SP2021$TotalMET.post),paired = T)

# -----motivation------

# currently the five likert scales are not labeled correctly. Data not usable.

# Select output-------
SP21 <- SP2021 %>% select(
  Name, Instructor,Gender,Age,ClassStat,Major,SchoolCombined,
  VigMET.pre,VigMET.post,ModMET.pre,ModMET.post,WalkMET.pre,WalkMET.post,
  TotalMET.pre,TotalMET.post,TotalMET.diff
  
)

write.csv(SP21,"21SP raw.csv",row.names = F)
