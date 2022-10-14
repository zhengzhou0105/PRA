# ACSM analysis utility funcitons
# Author: Zheng Zhou
# Date: 10/06/2021

# Load library------------
require(dplyr)
require(chron)
require(stringr)
require(ggplot2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyr)
library(reshape2)
library(readr)
library(tidyverse)

data("stop_words") 

# Global settings--------
set.seed(seed = seed)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Qualtrics cleaning-----

rmQualHeader <- function(input){   # input must be ACSMsurvey dataframe
  output <- input[-c(1,2),]
  return(output)
}

cleanByIP <- function(input){
  Nobs <- nrow(input)
  
  # Examine by IP address
  NuniqueIP <- length(unique(input$IPAddress))
  if(Nobs > NuniqueIP) {
    # Find records from duplicated and unique IP
    df_DupIP <- input %>% group_by(IPAddress) %>% filter(n() > 1)
    df_uniqueIP <- input %>% group_by(IPAddress) %>% filter(n() <= 1)
    
    # Save dupliated records
    write.csv(df_DupIP,"Records from duplicated IP address.csv",
              row.names = F)
    print("Records from duplicated IP address were saved in a separate file")
    
    # If duplicates from the same IP contains finished and unfinished
    # remove unfinished
    df_DupIP_fin <- df_DupIP %>% filter(Finished == "True")
    df_DupIP_unfin <- df_DupIP %>% filter(Finished == "False")
    
    # the IP address contain both finished and unfinished
    vec_IPdup_both <- df_DupIP_fin$IPAddress[df_DupIP_fin$IPAddress %in% df_DupIP_unfin$IPAddress]
    for(i in 1:length(vec_IPdup_both)){
      IPdup_both <- vec_IPdup_both[i]
      id_fin <- which(df_DupIP_fin$IPAddress == IPdup_both)
      id_unfin <- which(df_DupIP_unfin$IPAddress == IPdup_both)
      # remove unfinished obs if a finished obs is available from the save IP
      if(length(id_unfin > 0)){
        df_DupIP_unfin <- df_DupIP_unfin[-id_unfin,]
      } else {
        next
      }
    }
    # combine cleaned obs of duplicates
    df_dupIP_clean <- rbind(df_DupIP_fin,df_DupIP_unfin)
    # combine with obs from unique IP
    df_clean <- rbind(df_dupIP_clean,df_uniqueIP)
  } else {
    print("No duplicated records from the same IP address.")
    df_clean <- input
    return(df_clean)
  }
}

cleanByEmail <- function(input){
  # Take the latest obs from the same email
  # convert record time to time object
  # ConvertRecordTime require input of a single vector. lapply returns a list
  input$Time <- unlist(lapply(input$RecordedDate,function(x) convertRecordTime(x)))
  
  # Examine by email
  # find duplicate email in non NAs
  df_DupEmail <- input %>% filter(ContactInfo_2 != "") %>% group_by(ContactInfo_2) %>%
    filter(n() > 1)
  df_uniqueEmail <- rbind(input %>% filter(ContactInfo_2 == ""),
                          input %>% filter(ContactInfo_2 != "") %>% group_by(ContactInfo_2) %>%
                            filter(n() <= 1))
  print("Observations with duplicate emails were saved in a separate file")
  write.csv(df_DupEmail,"Records from duplicated emails.csv",row.names = F)
  
  # take only the latest obs
  df_DupEmail_clean <- data.frame()
  for(i in 1:length(unique(df_DupEmail$ContactInfo_2))){
    # examine record time per email
    Email <- df_DupEmail$ContactInfo_2[i]
    df_DupEmail_temp <- df_DupEmail %>% filter(ContactInfo_2 == Email)
    id_dupEmail <- which(df_DupEmail_temp$Time ==  max(df_DupEmail_temp$Time))
    df_DupEmail_keep <- df_DupEmail_temp[id_dupEmail,]
    df_DupEmail_clean <- rbind(df_DupEmail_clean,df_DupEmail_keep)
  }
  
  # combine records of unique email and cleaned email
  df_clean <- rbind(df_uniqueEmail,df_DupEmail_clean)
  return(df_clean)
}

convertRecordTime <- function(RecordTime){

    recordtime <- RecordTime
    date <- stringr::str_sub(recordtime,start = 1,end = 10)
    time <- stringr::str_sub(recordtime,start = 12,end = 20)
    chron(dates. = date, times. = time,
          format = c(dates = "y-m-d",
                     times = "h:m:s"))
}

ExtractFirstYear <- function(input){   # input be a string including at least a year
  tempYR <- str_extract(input,pattern = "\\d{4}?")
  YRoutput <- ifelse(as.numeric(tempYR) <= 2021, as.numeric(tempYR), NA)
}

WordcloudTM <- function(input){ # input be a vector of strings
  Nobs <- length(input)
  # df_text <- tibble(line = 1:Nobs,
  #                   text = input)
  # df_token <- df_text %>% unnest_tokens(
  #   output = word,input = text,
  #   token = "words"
  # )
  # df_token_clean <- df_token %>% anti_join(stop_words)
  # df_token_count <- df_token_clean %>% count(word,sort = T)
  # wordcloud(words = df_token_count$word, freq = df_token_count$n, min.freq = 1,
  #           max.words=200, random.order=FALSE, rot.per=0.35,
  #           colors=brewer.pal(8, "Dark2"))
  ## load text
  text <- VCorpus(VectorSource(input))
  
  ## text transformation
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
  
  # Remove punctuations
  text <- tm_map(text, removePunctuation)
  # Eliminate extra white spaces
  text <- tm_map(text, stripWhitespace)
  # Remove your own stop word
  # specify your stopwords as a character vector
  text <- tm_map(text, removeWords, c("feel", "think","like","also","able",
                                      "acsm","certification","cec",
                                      "recertif"))
  # Text stemming
  text <- tm_map(text, stemDocument)
  
  
  ## term-document matrix
  dtm <- TermDocumentMatrix(text)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)

  ## generate wordcloud
  print(
    wordcloud::wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                         max.words=100, random.order=FALSE, rot.per=0.35,
                         colors=brewer.pal(8, "Dark2"))
  )
}
