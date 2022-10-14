# ACSM customer survey analysis
# Authour: Zheng Zhou
# Date: 10/06/2021

# Settings---------
seed <- 47405

source("ACSM customer survey utility functions.R")        # Load utilities

# Input data-----------

# If load raw data, then run the cleaning
# If load clean data, do not run the cleaning again

# Raw data
ACSMsurvey_raw <- read.csv("2021 ACSM Continuing Education Survey_October 6, 2021 EST1545pm.csv")
Nnobs_raw <- nrow(ACSMsurvey_raw)      # number of raw records

# clean data (by IP and Email)
ACSMsurvey_clean <- read.csv("ACSM survey results clean.csv")

# Cleaning-----------

# clean data by different methods

# clean by IP address
ACSMsurvey_cleanIP <- ACSMsurvey_raw %>% rmQualHeader() %>% cleanByIP()

# clean by Email
ACSMsurvey_cleanEmail <- ACSMsurvey_raw %>% rmQualHeader() %>% cleanByEmail()

# clean by both IP and Email
ACSMsurvey_cleanIPEmail <- ACSMsurvey_raw %>% rmQualHeader() %>% 
  cleanByEmail() %>% cleanByIP()

# Export clean dataset
ACSMsurvey_clean <- ACSMsurvey_cleanEmail
write.csv(ACSMsurvey_clean,
          file = "ACSM survey results clean.csv", row.names = F)

# Descriptive Analysis------

# *Gender-------
table(ACSMsurvey_clean$Gender)
table(ACSMsurvey_clean$Gender_5_TEXT)

# *Age-------
table(ACSMsurvey_clean$Age)
ggplot(ACSMsurvey_clean,aes(x = Age))+
  geom_bar()+
  labs(x = "Age Group")

# *Country/ Region-------
table(ACSMsurvey_clean$Q4)

# *NonUS regions---------
df_temp <- ACSMsurvey_clean %>% filter(
  Q4 != "US"
)
table(df_temp$Q5)
ggplot(df_temp,aes(x = Q5))+
  geom_bar()+
  theme_classic(base_size = 7)+
  labs(title = "Non-US responses", x= "Country/Regions")

# *US States--------
df_temp <- ACSMsurvey_clean %>% filter(
  Q4 == "US"
)
df_plot <- table(df_temp$Q52) %>% reshape2::melt()
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

# *Race------
table(ACSMsurvey_clean$Q3)
ggplot(ACSMsurvey_clean,aes(x = Q3))+
  geom_bar()+
  theme_classic(base_size = 7)


# *Employment status------
table(ACSMsurvey_clean$Q6)
ggplot(ACSMsurvey_clean,aes(x = Q6))+
  geom_bar()+
  theme_classic(base_size = 7)

# *Employment setting--------

table(ACSMsurvey_clean$Q7)

df_temp <- ACSMsurvey_clean %>% filter(
  Q6 == "Full-time employment"
)
table(df_temp$Q7)

df_comp <- data.frame(
  all = table(ACSMsurvey_clean$Q7),
  FTE = table(df_temp$Q7)

)
rownames(df_comp) <- df_comp[,1]
rownames(df_comp)[1] <- "NA"
df_comp <- df_comp[,c(2,4)]
chisq.test(x = df_comp)

table(ACSMsurvey_clean$Q7_11_TEXT)

# *Year of exp-----
table(ACSMsurvey_clean$Q8)

# *Eduation------
table(ACSMsurvey_clean$Q9)
df_temp <- mutate(ACSMsurvey_clean,
                  Education = ifelse(Q9 == "",NA,
  ifelse(Q9 == "Other:","Others",
    ifelse(Q9 == "MasterÃ¢â¬â¢s degree","Masters",
      ifelse(Q9 == "Doctoral degree","Doctoral",
         ifelse(Q9 == "Associates degree" | Q9 == "BachelorÃ¢â¬â¢s degree" | Q9 == "Trade School Completion",
                "Undergrad or Professional", "High School"
            ))))))
table(df_temp$Education)

# *Annual income-------
table(ACSMsurvey_clean$Q10)

# *ACSM membership-----
table(ACSMsurvey_clean$Q34)

# *First year certified------
table(ACSMsurvey_clean$Q15)
df_temp <- ACSMsurvey_clean
df_temp <- mutate(df_temp,
                  FirstYRCert = ExtractFirstYear(Q15))
table(df_temp$FirstYRCert)
ggplot(df_temp,aes(x = FirstYRCert))+
  geom_bar()

# Certified in the last three years
df_temp <- df_temp %>% filter(
  FirstYRCert <= 2017
)
table(df_temp$Q76)

# *Last certified year------
table(ACSMsurvey_clean$Q85)

# *Re-certification satisfcation-----
table(ACSMsurvey_clean$Q12_1)
ggplot(ACSMsurvey_clean,aes(x = Q12_1))+
  geom_bar()
df_dissat <- filter(ACSMsurvey_clean,
                    Q12_1 <= 7)
write.csv(df_dissat,"Subjects felt unsatisifed about the re-certificaiton process.csv",
          row.names = F)

# *How to improve re-cert-----
WordcloudTM(ACSMsurvey_clean$Q45)

# *Favorite experience-----
WordcloudTM(ACSMsurvey_clean$Q13)

# *Current certification-------
ACSMsurvey_clean <- ACSMsurvey_clean %>% mutate(
  CPT = ifelse(Q20_1 == "", 0 , 1),
  GEI = ifelse(Q20_2 == "", 0 , 1),
  EP = ifelse(Q20_3 == "", 0 , 1),
  CEP = ifelse(Q20_4 == "", 0 , 1),
  SP = ifelse(Q20_5 == "", 0 , Q20_5_TEXT),
  NCERT = CPT + GEI + EP + CEP + ifelse(SP == 0, 0 , 1)
)
table(ACSMsurvey_clean$NCERT)
ggplot(ACSMsurvey_clean,aes(x = NCERT))+
  geom_bar()+
  scale_x_continuous(breaks = 0:5)
sum(ACSMsurvey_clean$CPT)
sum(ACSMsurvey_clean$GEI)
sum(ACSMsurvey_clean$EP)
sum(ACSMsurvey_clean$CEP)

# *Satisfy with CPT education------
df_temp <- ACSMsurvey_clean %>% dplyr::filter(
  CPT == 1
)
table(df_temp$Q64)
table(df_temp$Q63)

# *Satisfy with GEI EC-----
df_temp <- ACSMsurvey_clean %>% dplyr::filter(
  GEI == 1
)
table(df_temp$Q59)
table(df_temp$Q65)

# *Satisfy with EP EC-----
df_temp <- ACSMsurvey_clean %>% dplyr::filter(
  EP == 1
)
table(df_temp$Q72)
table(df_temp$Q73)

# *Satisfy with CEP EC-----
df_temp <- ACSMsurvey_clean %>% dplyr::filter(
  CEP == 1
)
table(df_temp$Q74)
table(df_temp$Q75)

# *Cert to take---------
table(ACSMsurvey_clean$Q70)
ggplot(ACSMsurvey_clean,aes(x = Q70))+
  geom_bar()+
  theme_classic(base_size = 7.5)
df_temp <- dplyr::filter(ACSMsurvey_clean,
                         Q70 == "ACSM-CEP")
sum(df_temp$CPT)
sum(df_temp$GEI)
sum(df_temp$EP)
sum(df_temp$CEP)

# *Most interested cert----
table(ACSMsurvey_clean$Q24)

# *Aware of online EC-----
table(ACSMsurvey_clean$Q89)

# *Content area most interested-----
table(ACSMsurvey_clean$Q71_1)
table(ACSMsurvey_clean$Q71_2)
table(ACSMsurvey_clean$Q71_3)
table(ACSMsurvey_clean$Q71_4)
table(ACSMsurvey_clean$Q71_5)
table(ACSMsurvey_clean$Q71_6)
table(ACSMsurvey_clean$Q71_7)

# *Interests in re-cert domains-----
table(ACSMsurvey_clean$Q54_1)
# ggplot(ACSMsurvey_clean,aes(x = Q54_1))+
#   geom_histogram()+
#   scale_x_continuous(breaks = 0:10)
table(ACSMsurvey_clean$Q54_2)
table(ACSMsurvey_clean$Q54_3)
table(ACSMsurvey_clean$Q54_4)
table(ACSMsurvey_clean$Q54_5)

# *Percent Post COVID EC---------
df_temp <- select(ACSMsurvey_clean,
                  CEC.PostCOVID_1_1,CEC.PostCOVID_2_1,CEC.PostCOVID_3_1,
                  CEC.PostCOVID_4_1,CEC.PostCOVID_5_1
                  ) %>% dplyr::filter(
                    !is.na(CEC.PostCOVID_1_1)
                  )
PostCOVIDECMax <- names(unlist(apply(df_temp,MARGIN = 1,function(x){
  which(x == max(x,na.rm = T))
})))
table(PostCOVIDECMax)


# *Percent during COVID EC---------
df_temp <- select(ACSMsurvey_clean,
                  Q28_1_1,Q28_2_1,Q28_3_1,
                  Q28_4_1,Q28_5_1
) %>% dplyr::filter(
  !is.na(Q28_1_1)
)
InCOVIDMax <- names(unlist(apply(df_temp,MARGIN = 1,function(x){
  which(x == max(x,na.rm = T))
})))
table(InCOVIDMax)

# *EC consideration----
table(ACSMsurvey_clean$Q30)
ggplot(ACSMsurvey_clean,aes(x = Q30))+
  geom_bar()+
  theme_classic(base_size = 8)

# *Course Price------
# 10-hour online course (10 contact hours) typically costs between $179 - $298 (USD)
table(ACSMsurvey_clean$Q80_NPS_GROUP)
table(ACSMsurvey_clean$Q81_NPS_GROUP)
table(ACSMsurvey_clean$Q82_NPS_GROUP)

# *Member with 10 free EC------
table(ACSMsurvey_clean$Q37_NPS_GROUP)

# *Important member benefit----
table(ACSMsurvey_clean$MmbrBenefitsInterest_1)
table(ACSMsurvey_clean$MmbrBenefitsInterest_2)
table(ACSMsurvey_clean$MmbrBenefitsInterest_3)
table(ACSMsurvey_clean$MmbrBenefitsInterest_4)
table(ACSMsurvey_clean$MmbrBenefitsInterest_5)
table(ACSMsurvey_clean$MmbrBenefitsInterest_6)
table(ACSMsurvey_clean$MmbrBenefitsInterest_7)

# *Most valuable ACSM membership------
df_temp <- dplyr::filter(ACSMsurvey_clean,
                         Q34 == "Yes") %>% dplyr::filter(
                           !MostValMembership %in% c("No response","No response ",
                                                     "no response","no response ",
                                                     "no responses") 
                         )
WordcloudTM(df_temp$MostValMembership)

# *Recommend ACSM cert-----
table(ACSMsurvey_clean$ACSMcertRecc_1)
ggplot(ACSMsurvey_clean,aes(x = ACSMcertRecc_1))+
  geom_bar()+
  scale_x_continuous(breaks = 0:10)

# *Improve ACSM certification---
df_temp <- dplyr::filter(ACSMsurvey_clean,
                         ACSMcertRecc_1 <= 5)
WordcloudTM(df_temp$Improve)

# *Biggest need---
df_temp <- mutate(ACSMsurvey_clean,
                  BiggestNeed = str_replace(BiggestNeed,"none\\s+","none"),
                  BiggestNeed = str_replace(BiggestNeed,"None\\s+","none")
                  ) %>% dplyr::filter(
                    BiggestNeed != "none"
                  )
WordcloudTM(df_temp$BiggestNeed)

# Inferential-----------
df_temp <- mutate(ACSMsurvey_clean,
                  income = ifelse(Q10 == "Under $20,000",10000,
                                  ifelse(Q10 == "$20,001 â€“ $40,000",30000,
                                         ifelse(Q10 == "$40,001 â€“ $60,000",50000,
                                                ifelse(Q10 == "$60,001 â€“ $80,000", 70000,
                                                       ifelse(Q10 == "$80,001 â€“ $100,000",90000,
                                                              ifelse(Q10 == "$100,001 or over",120000,NA))))))
)
s
summary(df_temp$income)

ModIncome <- lm(data = df_temp,
                income ~ Gender + Age + Q3 + Q4 + Q52 + Q6 + Q7 + Q8 + Q9+
      CPT + GEI + EP + CEP + Q34
    )
summary(ModIncome)

# Output-------
rmarkdown::render(input  = "ACSM survey analysis report.rmd",
                  output_format = "pdf_document")
