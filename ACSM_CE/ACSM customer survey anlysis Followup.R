# ACSM customer survey analysis Followup
# Author: Zheng Zhou

# Settings---------
seed <- 47405

source("ACSM customer survey utility functions.R")        # Load utilities

# Input and Cleaning-------
# clean data (by IP and Email)
ACSMsurvey_clean <- read.csv("ACSM survey results clean Feb 10 2022.csv")

ACSM_clean_certi <- ACSMsurvey_clean %>% mutate(
  FirstYRCerti = ExtractFirstYear(Q15)
) %>% dplyr::filter(
  !is.na(FirstYRCerti)
) %>% mutate(
  Income_median = ifelse(Q10 == "Prefer not to say",NA,
                         ifelse(Q10 == "Under $20,000",10000,
                                ifelse(Q10 == "$20,001 - $40,000",30000,
                                       ifelse(Q10 == "$40,001 - $60,000",50000,
                                              ifelse(Q10 == "$60,001 - $80,000",70000,
                                                     ifelse(Q10 == "$80,001 - $100,000",90000,
                                                            130000))))))
)

# Description of Demographics and SES-----

# Gender
ACSM_clean_certi <- ACSM_clean_certi %>% mutate(
  Gender_ = ifelse(Gender == "Female","Female",ifelse(Gender == "Male","Male","Other"))
)
table(ACSM_clean_certi$Gender_)

# Age
ACSM_clean_certi <- ACSM_clean_certi %>% mutate(
  Age_ = ifelse(Age == "Prefer not to say","Other",Age)
)
table(ACSM_clean_certi$Age_)

# US/International
table(ACSM_clean_certi$Q4)

# States and Map
ACSM_clean_certi %>% filter(
  !Q4 == "US"
) %>% count(Q5)

# US states
df_plot <- ACSM_clean_certi %>% filter(
  Q4 == "US"
)
df_plot <- table(df_plot$Q52) %>% reshape2::melt()
colnames(df_plot) <- c("State","Count")
df_plot <- df_plot %>% dplyr::arrange(desc(Count))
print(df_plot)

df_plot$region <- tolower(df_plot$State)
states <- map_data("state")
map.df <- merge(states,df_plot, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=Count))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()

# Race
table(ACSM_clean_certi$Q3)
ACSM_clean_certi <- ACSM_clean_certi %>%rowwise() %>% mutate(
  Race = ifelse(Q3 == "Caucasian","Caucasian",
                ifelse(Q3 == "African American","African/ African American",
                       ifelse(Q3 == "Asian" | Q3 == "Native American" | Q3 == "Native Hawaiian or Pacific Islander",
                              "Asian, Native American and Pacific Islander",
                              ifelse(Q3 == "Latino or Hispanic","Latino",
                                     "Multiracial and Others"))))
)
table(ACSM_clean_certi$Race)

# Employment Status
table(ACSM_clean_certi$Q6)

# Employment setting
table(ACSM_clean_certi$Q7)

ACSM_clean_certi %>% filter(
  Q6 == "Full-time employment"
) %>% count(Q7)

# Years of Experience in field
table(ACSM_clean_certi$Q8)

# Education 
table(ACSM_clean_certi$Q9)
ACSM_clean_certi <- mutate(ACSM_clean_certi,
                  Education = ifelse(Q9 == "",NA,
                                     ifelse(Q9 == "Other:","Others",
                                            ifelse(Q9 == "Master's degree","Masters",
                                                   ifelse(Q9 == "Doctoral degree","Doctoral",
                                                          ifelse(Q9 == "Associates degree" | Q9 == "Bachelor's degree" | Q9 == "Trade School Completion",
                                                                 "Undergrad or Professional", "High School"
                                                          ))))))
table(ACSM_clean_certi$Education)


# Annual income
summary(ACSM_clean_certi$Income_median)

# ACSM membership
table(ACSM_clean_certi$Q34)

# First Year Cetified 
table(ACSM_clean_certi$FirstYRCerti)

# Recertified in last three years
table(ACSM_clean_certi$Q76)

# Time of latest certification
ACSM_clean_certi %>% filter(
  !Q85 == "" & !Q85 == "Did not recertify" 
) %>% count(Q85)

# Satisfaction about re-certification
hist(ACSM_clean_certi$Q12_1,
     main = "Satisfaction about re-certification")

# Current Certification
ACSM_clean_certi <- ACSM_clean_certi %>% mutate(
  CPT = ifelse(Q20_1 == "", 0 , 1),
  GEI = ifelse(Q20_2 == "", 0 , 1),
  EP = ifelse(Q20_3 == "", 0 , 1),
  CEP = ifelse(Q20_4 == "", 0 , 1),
  SP = ifelse(Q20_5 == "", 0 , 1),
  NCERT = CPT + GEI + EP + CEP + SP
)

ACSM_clean_certi %>% filter(
  !NCERT == 0
) %>% count(NCERT)

# What kind of specialty certification
ACSM_clean_certi %>% filter(
  SP == 1
) %>% count(Q20_5_TEXT)

# Income by Certification Number
ACSM_clean_certi %>% group_by(NCERT,Income_median) %>% summarise(n = n()) %>% mutate(
  freq= n /sum(n)
) %>% print(n = 30)

summary(lm(Income_median ~ NCERT, data = ACSM_clean_certi))

# Demographics of Holders of each type of certification-----
# Holders of CPT
ACSM_clean_certi %>% filter(           # gender
  CPT == 1
) %>% count(Gender_)

ACSM_clean_certi %>% filter(           # age
  CPT == 1
) %>% count(Age_)

ACSM_clean_certi %>% filter(          # US states
  CPT == 1
) %>% count(States = Q52)

ACSM_clean_certi %>% filter(          # employment status
  CPT == 1
) %>% count(Employment = Q6)

ACSM_clean_certi %>% filter(          # workplace
  CPT == 1
) %>% count(Workplace = Q7)
 
ACSM_clean_certi %>% filter(          # years of experience
  CPT == 1
) %>% count(YearsExperience = Q8)

ACSM_clean_certi %>% filter(           # education
  CPT == 1
) %>% count(Education = Education)

ACSM_clean_certi %>% filter(            # median income
  CPT == 1
) %>% count(Income_median)

ACSM_clean_certi %>% filter(           # have ACSM membership
  CPT == 1
) %>% count(ACSMmember = Q34)

ACSM_clean_certi %>% filter(
  CPT == 1
) %>% count(Ncertification = NCERT)    # number of certification in hand

ACSM_clean_certi %>% filter(
  CPT == 1
) %>% count(WANTCERT = Q70)           # Certification to take


# Holders of GEI
ACSM_clean_certi %>% filter(           # gender
  GEI == 1
) %>% count(Gender_)

ACSM_clean_certi %>% filter(           # age
  GEI == 1
) %>% count(Age_)

ACSM_clean_certi %>% filter(          # US states
  GEI == 1
) %>% count(States = Q52)

ACSM_clean_certi %>% filter(          # employment status
  GEI == 1
) %>% count(Employment = Q6)

ACSM_clean_certi %>% filter(          # workplace
  GEI == 1
) %>% count(Workplace = Q7)

ACSM_clean_certi %>% filter(          # years of experience
  GEI == 1
) %>% count(YearsExperience = Q8)

ACSM_clean_certi %>% filter(           # education
  GEI == 1
) %>% count(Education = Education)

ACSM_clean_certi %>% filter(            # median income
  GEI == 1
) %>% count(Income_median)

ACSM_clean_certi %>% filter(           # have ACSM membership
  GEI == 1
) %>% count(ACSMmember = Q34)

ACSM_clean_certi %>% filter(
  GEI == 1
) %>% count(Ncertification = NCERT)    # number of certification in hand

ACSM_clean_certi %>% filter(
  GEI == 1
) %>% count(WANTCERT = Q70)           # Certification to take


# Holders of EP
ACSM_clean_certi %>% filter(           # gender
  EP == 1
) %>% count(Gender_)

ACSM_clean_certi %>% filter(           # age
  EP == 1
) %>% count(Age_)

ACSM_clean_certi %>% filter(          # US states
  EP == 1
) %>% count(States = Q52)

ACSM_clean_certi %>% filter(          # employment status
  EP == 1
) %>% count(Employment = Q6)

ACSM_clean_certi %>% filter(          # workplace
  EP == 1
) %>% count(Workplace = Q7)

ACSM_clean_certi %>% filter(          # years of experience
  EP == 1
) %>% count(YearsExperience = Q8)

ACSM_clean_certi %>% filter(           # education
  EP == 1
) %>% count(Education = Education)

ACSM_clean_certi %>% filter(            # median income
  EP == 1
) %>% count(Income_median)

ACSM_clean_certi %>% filter(           # have ACSM membership
  EP == 1
) %>% count(ACSMmember = Q34)

ACSM_clean_certi %>% filter(
  EP == 1
) %>% count(Ncertification = NCERT)    # number of certification in hand

ACSM_clean_certi %>% filter(
  EP == 1
) %>% count(WANTCERT = Q70)           # Certification to take

# Holders of CEP
ACSM_clean_certi %>% filter(           # gender
  CEP == 1
) %>% count(Gender_)

ACSM_clean_certi %>% filter(           # age
  CEP == 1
) %>% count(Age_)

ACSM_clean_certi %>% filter(          # US states
  CEP == 1
) %>% count(States = Q52)

ACSM_clean_certi %>% filter(          # employment status
  CEP == 1
) %>% count(Employment = Q6)

ACSM_clean_certi %>% filter(          # workplace
  CEP == 1
) %>% count(Workplace = Q7)

ACSM_clean_certi %>% filter(          # years of experience
  CEP == 1
) %>% count(YearsExperience = Q8)

ACSM_clean_certi %>% filter(           # education
  CEP == 1
) %>% count(Education = Education)

ACSM_clean_certi %>% filter(            # median income
  CEP == 1
) %>% count(Income_median)

ACSM_clean_certi %>% filter(           # have ACSM membership
  CEP == 1
) %>% count(ACSMmember = Q34)

ACSM_clean_certi %>% filter(
  CEP == 1
) %>% count(Ncertification = NCERT)    # number of certification in hand

ACSM_clean_certi %>% filter(
  CEP == 1
) %>% count(WANTCERT = Q70)           # Certification to take


# # Output-------
rmarkdown::render(input  = "Revision to ACSM Survey Report Dec 6 2021.rmd",
                  output_format = "word_document")
