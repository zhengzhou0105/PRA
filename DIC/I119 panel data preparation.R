# Prepare I119 panel data
# Author: Zheng Zhou
# Date: 09/03/2021

# Settings---------
seed <- 47405
iter <- 1000


source("I119 panel data utility functions.R")

# Input Raw I119 Data-------
# Data are stored by term in a Rdata file
load("I119 panel raw data.Rdata")

# read from csv
S19FA <- read.csv("S19FA clean.csv")
S20FA <- read.csv("S20FA clean.csv")
S20SP <- read.csv("S20SP clean.csv")
S21SP <- read.csv("S21SP clean.csv")

# Cleaning Raw Data------

# Put together pre and post datasets with term being a new variable

# List of Variables to keep: term, survey date, subject name(IU email), Coach,
# PA before I119, PA after I119, PA change, sex, age, major, class

# merge by terms
S19FA <- MergeByName(S19FAPre,S19FAPost) %>% KeepPAData()

S20SP <- MergeByName(S20SPPre,S20SPPost) %>% KeepPAData()

S20FA <- S20FA %>% select(
  Name,
  Course.pre,Course.post,
  Gender,Age,
  ClassStat,Major,SchoolCombined,
  VigMET.pre,VigMET.post,ModMET.pre,ModMET.post,
  WalkMET.pre,WalkMET.post,TotalMET.pre,TotalMET.post,
  TotalMET.diff
)

colnames(S21SP)[which(colnames(S21SP) == "Q185")] <- "Gender"
colnames(S21SP)[which(colnames(S21SP) == "Q186")] <- "Age"
colnames(S21SP)[which(colnames(S21SP) == "Q187")] <- "ClassStat"
colnames(S21SP)[which(colnames(S21SP) == "Q188")] <- "SchoolCombined"
colnames(S21SP)[which(colnames(S21SP) == "Q189")] <- "Major"

S21SP <- S21SP %>% select(
  Name,
  Instructor,PrimInstructor,
  Gender,Age,
  ClassStat,Major,SchoolCombined,
  VigMET.pre,VigMET.post,ModMET.pre,ModMET.post,
  WalkMET.pre,WalkMET.post,TotalMET.pre,TotalMET.post
) 
S21SP <- S21SP %>% mutate(
  TotalMET.diff = TotalMET.post - TotalMET.pre
)


# Studnet Last Name
S19FA <- S19FA %>% mutate(
  LastName = GetLastName(Name),
  .after = Name
) 
S19FA <- S19FA %>% group_by(LastName) %>% arrange(LastName)


S20FA <- S20FA %>% mutate(
  LastName = GetLastName(Name),
  .after = Name
)
S20FA <- S20FA %>% group_by(LastName) %>% arrange(LastName)


S20SP <- S20SP %>% mutate(
  LastName = GetLastName(Name),
  .after = Name
)
S20SP <- S20SP %>% group_by(LastName) %>% arrange(LastName)

S21SP <- S21SP %>% mutate(
  LastName = GetLastName(Name),
  .after = Name
)
S21SP <- S21SP %>% group_by(LastName) %>% arrange(LastName)


# Coaching sessions
# Rename pre and post
S19FA <- rename(S19FA,
                Course.pre = Course,
                Course.post = PostCourse)
S20SP <- rename(S20SP,
                Course.pre = Course,
                Course.post = PostCourse
                )
S21SP <- rename(S21SP,
                Session = Instructor,
                Course.pre = PrimInstructor)

# if either pre or post session is missing, use another
# if pre and post are different, as Session changed
S19FA <- S19FA %>% mutate(
  Course.post = ifelse(is.na(Course.post),Course.pre,Course.post),
  Course.pre = ifelse(is.na(Course.pre),Course.post,Course.pre),
  Session = ifelse(Course.pre == Course.post,Course.post,"Session Changed"),
  .after = Course.post
)

S20FA <- S20FA %>% mutate(
  Course.post = ifelse(is.na(Course.post),Course.pre,Course.post),
  Course.pre = ifelse(is.na(Course.pre),Course.post,Course.pre),
  Session = ifelse(Course.pre == Course.post,Course.post,"Session Changed"),
  .after = Course.post
)
S20SP <- S20SP %>% mutate(
  Course.post = ifelse(is.na(Course.post),Course.pre,Course.post),
  Course.pre = ifelse(is.na(Course.pre),Course.post,Course.pre),
  Session = ifelse(Course.pre == Course.post,Course.post,"Session Changed"),
  .after = Course.post
)

S21SP <- S21SP %>% mutate(
  Course.post = Course.pre
)

# mannually check lastname in excel
write.csv(S19FA,"S19FA clean.csv",row.names = F)
write.csv(S20FA,"S20FA clean.csv",row.names = F)
write.csv(S20SP,"S20SP clean.csv",row.names = F)
write.csv(S21SP,"S21SP clean.csv",row.names = F)

# order by session - lastname
S19FA <- arrange(S19FA,
                 Session,LastName)
S20FA <- arrange(S20FA,
                 Session,LastName)
S20SP <- arrange(S20SP,
                 Session,LastName)
S21SP <- arrange(S21SP,
                 Session,LastName)

# calculate MET for organized data
S19FA <- S19FA %>% rowwise() %>% mutate(
                TotalMET.pre = sum(VigMET.pre,ModMET.pre,WalkMET.pre,na.rm = T),
                TotalMET.post = sum(VigMET.post, ModMET.post, WalkMET.post,na.rm =T),
                TotalMET.diff = TotalMET.post - TotalMET.pre
                ) %>% mutate(
                  Coach = "",
                  .after = Session
                )
S20FA <- S20FA %>% rowwise() %>% mutate(
  TotalMET.pre = sum(VigMET.pre,ModMET.pre,WalkMET.pre,na.rm = T),
  TotalMET.post = sum(VigMET.post, ModMET.post, WalkMET.post,na.rm =T),
  TotalMET.diff = TotalMET.post - TotalMET.pre
) %>% mutate(
  Coach = "",
  .after = Session
)
S20SP <- S20SP %>% rowwise() %>% mutate(
  TotalMET.pre = sum(VigMET.pre,ModMET.pre,WalkMET.pre,na.rm = T),
  TotalMET.post = sum(VigMET.post, ModMET.post, WalkMET.post,na.rm =T),
  TotalMET.diff = TotalMET.post - TotalMET.pre
) %>% mutate(
  Coach = "",
  .after = Session
)
S21SP <- S21SP %>% rowwise() %>% mutate(
  TotalMET.pre = sum(VigMET.pre,ModMET.pre,WalkMET.pre,na.rm = T),
  TotalMET.post = sum(VigMET.post, ModMET.post, WalkMET.post,na.rm =T),
  TotalMET.diff = TotalMET.post - TotalMET.pre
) %>% mutate(
  Coach = "",
  .after = Session
)

# Input CKA Marked Data----

# Before input, make sure the variables are renamed 
# by the list in notes
S19FA <- read_xlsx("S19FA clean cka complete.xlsx")
S20SP <- read_xlsx("S20SP clean cka data.xlsx",sheet = "Sheet1")
S20FA <- read_xlsx("S20FA clean cka complete.xlsx")
S21SP <- read_xlsx("S21SP clean CKA complete.xlsx",sheet = "Sheet1")

# Remove Dropped Students
S19FA.clean <- S19FA %>% filter(
  Coach != "Dropped"
)
S20SP.clean <- S20SP %>% filter(
  Coach != "Dropped"
)
S20FA.clean <- S20FA %>% filter(
  Coach != "Dropped"
)
S21SP.clean <- S21SP %>% filter(
  Coach != "Dropped"
)



# Design Matrix Recoding-------
# Add Coach Index
S19FA.masked <- mutate(S19FA.clean,
  CoachID = ifelse(Coach == "Anna",1,
                   ifelse(Coach == "Chris",2,
                          ifelse(Coach == "Elena",3,
                                 ifelse(Coach == "Lanie",4,
                                        ifelse(Coach == "Matt",5,
                                               ifelse(Coach == "Shellie",6,
                                                      ifelse(Coach == "Zach",7,
                                                             ifelse(Coach == "Dani",8,
                                                                    ifelse(Coach == "Kyle",9,0))))))))),
  .after = Coach
                      )

S20SP.masked <- mutate(S20SP.clean,
                       CoachID = ifelse(Coach == "Anna",1,
                                        ifelse(Coach == "Chris",2,
                                               ifelse(Coach == "Elena",3,
                                                      ifelse(Coach == "Lanie",4,
                                                             ifelse(Coach == "Matt",5,
                                                                    ifelse(Coach == "Shellie",6,
                                                                           ifelse(Coach == "Zach",7,
                                                                                  ifelse(Coach == "Dani",8,
                                                                                         ifelse(Coach == "Kyle",9,0))))))))),
                       .after = Coach
) 

S20FA.masked <- mutate(S20FA.clean,
                       CoachID = ifelse(Coach == "Anna",1,
                                        ifelse(Coach == "Chris",2,
                                               ifelse(Coach == "Elena",3,
                                                      ifelse(Coach == "Lanie",4,
                                                             ifelse(Coach == "Matt",5,
                                                                    ifelse(Coach == "Shellie",6,
                                                                           ifelse(Coach == "Zach",7,
                                                                                  ifelse(Coach == "Dani",8,
                                                                                         ifelse(Coach == "Kyle",9,0))))))))),
                       .after = Coach
)

S21SP.masked <- mutate(S21SP.clean,
                       CoachID = ifelse(Coach == "Anna",1,
                                        ifelse(Coach == "Chris",2,
                                               ifelse(Coach == "Elena",3,
                                                      ifelse(Coach == "Lanie",4,
                                                             ifelse(Coach == "Matt",5,
                                                                    ifelse(Coach == "Shellie",6,
                                                                           ifelse(Coach == "Zach",7,
                                                                                  ifelse(Coach == "Dani",8,
                                                                                         ifelse(Coach == "Kyle",9,0))))))))),
                       .after = Coach
)


# Fix data types
# Inspection by dataset
S19FA.masked <- S19FA.masked %>% mutate(
  CoachID = as.character(CoachID),
  Age = as.numeric(Age),
  VigMET.pre = as.numeric(VigMET.pre),
  VigMET.post = as.numeric(VigMET.post),
  ModMET.pre = as.numeric(ModMET.pre),
  ModMET.post = as.numeric(ModMET.post),
  WalkMET.pre = as.numeric(WalkMET.pre),
  WalkMET.post = as.numeric(WalkMET.post)
)

S20SP.masked <- S20SP.masked %>% mutate(
  CoachID = as.character(CoachID),
  Age = as.numeric(Age),
  VigMET.pre = as.numeric(VigMET.pre),
  VigMET.post = as.numeric(VigMET.post),
  ModMET.pre = as.numeric(ModMET.pre),
  ModMET.post = as.numeric(ModMET.post),
  WalkMET.pre = as.numeric(WalkMET.pre),
  WalkMET.post = as.numeric(WalkMET.post)
)

S20FA.masked <- S20FA.masked %>% mutate(
  CoachID = as.character(CoachID),
  Age = as.numeric(Age),
  VigMET.pre = as.numeric(VigMET.pre),
  VigMET.post = as.numeric(VigMET.post),
  ModMET.pre = as.numeric(ModMET.pre),
  ModMET.post = as.numeric(ModMET.post),
  WalkMET.pre = as.numeric(WalkMET.pre),
  WalkMET.post = as.numeric(WalkMET.post)
)

S21SP.masked <- S21SP.masked %>% mutate(
  CoachID = as.character(CoachID)
)

S21SP.masked %>% group_by(CoachID) %>% 
  dplyr::summarise(Mean = mean(TotalMET.diff),
                   SD = sd(TotalMET.diff))

S20SP.masked %>% group_by(CoachID) %>% 
  dplyr::summarise(Mean = mean(TotalMET.diff),
                   SD = sd(TotalMET.diff))

S20FA.masked %>% group_by(CoachID) %>% 
  dplyr::summarise(Mean = mean(TotalMET.diff),
                   SD = sd(TotalMET.diff))

S19FA.masked %>% group_by(CoachID) %>% 
  dplyr::summarise(Mean = mean(TotalMET.diff),
                   SD = sd(TotalMET.diff))

# Merge into panel data
df_I119Merged <- bind_rows(S19FA.masked,S20SP.masked,S20FA.masked,S21SP.masked,
                           .id = "Term")
df_I119Merged <- mutate(df_I119Merged,
                        Term = as.character(Term))


# Two-stage Probabilistic Factor on Coach Factors-----

# To skip the process above, read from directory
df_posterior_lme_cov_stan_Coach <- read.csv("Posteriors of Predicted Coach Specific Effects.csv")

df_posterior_coach_long  <- reshape2::melt(df_posterior_lme_cov_stan_Coach) %>% 
  rename(Coach = variable,
         y = value) %>%
  mutate(
    Coach = as.numeric(stringr::str_sub(as.character(Coach),6,6))
  )

# Combine with factors
df_coachinfo <- readxl::read_xlsx("New Microsoft Excel Worksheet.xlsx")
df_coachinfo <- df_coachinfo %>% 
  mutate(Coach = ifelse(Name == "Anna",1,
                          ifelse(Name == "Chris",2,
                                 ifelse(Name == "Elena",3,
                                        ifelse(Name == "Lanie",4,
                                               ifelse(Name == "Matt",5,
                                                      ifelse(Name == "Shellie",6,
                                                             ifelse(Name == "Zach",7,
                                                                    ifelse(Name == "Dani",8,
                                                                           ifelse(Name == "Kyle",9,0))))))))),
         .after = Name)

df_coach_factor_merge <- left_join(x = df_posterior_coach_long,
                                  y = df_coachinfo,
                                  by = "Coach")
df_coach_factor_long <- df_coach_factor_merge %>% select(-Name)

# Output---------
save(
  S19FA.masked,S20SP.masked,S20FA.masked,S21SP.masked,df_I119Merged,
  df_coach_factor_long,
  file = "I119 Panel Data Clean.Rdata"
)
