# Analysis on COVID related text questions from I119 Qualtrics survey
# by Zheng Zhou
# Date: 10/30/2020

# global settings--------
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

# Question contents and narrative are different across survey waves
# So analysis by wave


# SP20 data---------
# load post data
SP20Post <- read.csv("I119 20SP post-survey qualtrics.csv",           ## read data
         header = T)
SP20Post <- SP20Post[c(3:241),]                                        ## remove header notions
nSP20 <- nrow(SP20Post)
rownames(SP20Post) <- 1:SP20Post

SP20PostName <- SP20Post[,"PostName"]                                  ## name of students
SP20PostCovid1 <- SP20Post[,"PostCOVonline"]                           ## After moving the course ONLINE due to COVID-19, what do you FEEL went well for you?
SP20PostCovid2 <- SP20Post[,"PostCOV2Things"]                          ## Please list TWO things we could do better if this course had to go ONLINE again next semester?


# analyze COVID 2 things using tm------

## load text
text <- VCorpus(VectorSource(SP20PostCovid2))

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
# Remove punctuations
text <- tm_map(text, removePunctuation)
# Eliminate extra white spaces
text <- tm_map(text, stripWhitespace)
# Remove your own stop word
# specify your stopwords as a character vector
text <- tm_map(text, removeWords, c("think","maybe","like","also"))
# # Text stemming
# text2 <- tm_map(text2, stemDocument)

## term-document matrix
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

## generate wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))



# analyze COVID online course using tm------

## load text
text <- VCorpus(VectorSource(SP20PostCovid1))

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
text <- tm_map(text, removeWords, c("feel", "think","like","also","able"))
# Text stemming
text <- tm_map(text, stemDocument)


## term-document matrix
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

## generate wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))




# analyze COVID online course using tidytext-------------
SP20PostCovid1_df <- tibble(line = 1:nSP20,                           ## convert the text (stored in a vector) to dataframe
                            text = SP20PostCovid1)
SP20PostCovid1_token <- SP20PostCovid1_df %>% unnest_tokens(output = word,         ## tokenization
                                    input = text,
                                    token = "words")
data("stop_words")                                                    ## clean stop words using the database stop words
SP20PostCovid1_clean <-  SP20PostCovid1_token %>% anti_join(stop_words) %>%
SP20PostCovid1_count <- SP20PostCovid1_clean %>% count(word, sort = T)
head(SP20PostCovid1_count,20) 

wordcloud(words = SP20PostCovid1_count$word, freq = SP20PostCovid1_count$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

# analyze covid two things using tidytext------
SP20PostCovid2_df <- tibble(line = 1:nSP20,
                            text = SP20PostCvoid2)



# FA20 data----------
FA20Pre <- read.csv("I119 20FA pre survey qualtrics.csv",
                    
                 header = T)
FA20Pre <- FA20Pre[c(9:282),]                    # the first 8 rows are headline and test
nFA20Pre <- nrow(FA20Pre)
rownames(FA20Pre) <- 1:nFA20Pre

FA20PreName <- FA20Pre[,"Name"]                  # name of the student
FA20PrePosi1 <- FA20Pre[,"Q58_1"]               # TWO positive things that have resulted from the COVID-19 pandemic experience for you personally?
FA20PrePosi2 <- FA20Pre[,"Q58_2"]
FA20PreNega1 <- FA20Pre[,"Q60_1"]          # TWO NEGATIVE things that have resulted from the COVID-19 pandemic experience for you personally
FA20PreNega2 <- FA20Pre[,"Q60_2"]
FA20PreMovePre1 <- FA20Pre[,"Q59_1"]        #  TWO words that describe your movement and/or exercise experiences BEFORE COVID-19 entered your life?
FA20PreMovePre2 <- FA20Pre[,"Q59_2"]
FA20PreMovePost1 <- FA20Pre[,"Q57_1"]    #  TWO words that describe your movement and/or exercise experiences AFTER COVID-19 entered your life? 
FA20PreMovePost2 <- FA20Pre[,"Q57_2"]
FA20PreMoveHelp1 <- FA20Pre[,"Q70_1"]   # What are the top THREE things we can do to help you move more throughout  COVID?
FA20PreMoveHelp2 <- FA20Pre[,"Q70_2"]
FA20PreMoveHelp3 <- FA20Pre[,"Q70_4"]

FA20PrePosiTwo <- paste(FA20PrePosi1,",", FA20PrePosi2)
FA20PreNegaTwo <- paste(FA20PreNega1,",",FA20PreNega2)
FA20PreMovePre <- paste(FA20PreMovePre1,",",FA20PreMovePre2)
FA20PreMovePost <- paste(FA20PreMovePost1,",",FA20PreMovePost2)
FA20PreMoveHelp <- paste(FA20PreMoveHelp1,",",FA20PreMoveHelp2,",",FA20PreMoveHelp3)

# analyze two positive things using tm----
text <- FA20PrePosiTwo %>% VectorSource() %>% VCorpus()

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
text <- tm_map(text, removeWords, c("got", "able"))
# # Text stemming
# text <- tm_map(text, stemDocument)

# output statistics
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = T)
d <- data.frame(word = names(v) , freq = v)
head(d,20)

# wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
  
# analyze two negative things using tm-----
text <- FA20PreNegaTwo %>% VectorSource() %>% VCorpus()

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
text <- tm_map(text, removeWords, c("less", "much","get","covid"))
# Text stemming
text <- tm_map(text, stemDocument)

# output statistics
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing = T)
d <- data.frame(word = names(v) , freq = v)
head(d,20)

# wordcloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
# analyze movement  before COVID using tm------
text <- FA20PreMovePre %>% VectorSource() %>% VCorpus()
  
# Convert the text to lower case
text <- tm_map(text, content_transformer(tolower))

# Remove punctuations
text <- tm_map(text, removePunctuation)

# Strip white space
text <- tm_map(text, stripWhitespace)

# build the dtm
dtm <- DocumentTermMatrix(text)
m <- as.matrix(dtm)
v <- rowSums(m)
d <- data.frame(word = names(v), freq = v)
head(d,20)

# analyze movement during COVID------
# analyze help for movement-------
# analyze movement before COVID using tidytext-------
text <- tibble(line = 1:nFA20Pre,                              ## convert to table with response index
               text = FA20PreMovePre) 
text_token <- text %>% unnest_tokens(output = word, input = text)        ## tokenization
text_count <- text_token %>% count(word, sort = T)              ## count
head(text_count,10)

text_sentiment_afinn <- text_token %>% inner_join(get_sentiments("afinn")) %>%          ## using afinn lexicon for sentiment
  group_by(index = line %/%  1) %>%
    summarize(sentiment = sum(value)) %>%
    mutate(method = "AFINN")
# text_sentiment_bing <- text_token %>% inner_join(get_sentiments("bing")) %>%         ## Bing and nrc lexicon
#   mutate(method = "Bing et al.")
# text_sentiment_nrc <- text_token %>% inner_join(get_sentiments("nrc")) %>%
#   filter(sentiment %in% c("positive","negative")) %>%
#   mutate(method = "NRC")
# text_sentiment_binary <- bind_rows(text_sentiment_bing,
#                                    text_sentiment_nrc) %>%
# count(method, index = line %/% 1 , sentiment) %>% 
#   spread(sentiment, n , fill = 0) %>% 
#   mutate(sentiment = positive - negative)
# 
# text_sentiment <- bind_rows(text_sentiment_afinn,
#                             text_sentiment_binary) 
# text_sentiment %>% ggplot(aes(x=index, y = sentiment , fill=method))+
#   geom_col(show.legend = F)+
#   facet_wrap(~method, ncol = 1,scales = "free_y")

# add afinn sentiment to data
FA20Pre$MovePreSentiment <- NA
for(i in 1:nrow(text_sentiment_afinn)){
  index <- text_sentiment_afinn$index[i]
  y <- text_sentiment_afinn$sentiment[i]
  FA20Pre$MovePreSentiment[index] <- y
}


# analyze movement during COVID using tidytext-----
text <- tibble(line = 1: nFA20Pre,
               text = FA20PreMovePost)
text_token <- text %>% unnest_tokens(output = word, input = text)
text_count <- text_token %>% count(word, sort = T)
head(text_count,10)

text_sentiment_afinn <- text_token %>% inner_join(get_sentiments("afinn")) %>%
  group_by(index = line %/% 1) %>%
  summarize(sentiment = sum(value)) %>%
  mutate(method ="AFINN")

FA20Pre$MovePostSentiment <- NA
for(i in 1:nrow(text_sentiment_afinn)){
  index <- text_sentiment_afinn$index[i]
  y <- text_sentiment_afinn$sentiment[i]
  FA20Pre$MovePostSentiment[index] <- y
}

# analyze movement sentiment pre and post COVID-----
FA20Pre$MoveDiffSentiment <- FA20Pre$MovePostSentiment - FA20Pre$MovePreSentiment

write.csv(FA20Pre,"FA20Pre.csv",row.names = F)
table(FA20Pre$MoveDiffSentiment)
hist(FA20Pre$MoveDiffSentiment)

write.csv(x = data.frame(name = FA20Pre$Name,
                         MoveBeforeCovid = FA20PreMovePre,
                         MoveDuringCovid = FA20PreMovePost,
                         Sentiment_MoveBeforeCovid = FA20Pre$MovePreSentiment,
                         Sentiment_MoveDuringCovid = FA20Pre$MovePostSentiment,
                         Sentiment_MoveChange = FA20Pre$MoveDiffSentiment),
          "Sentiment_Movement before and during COVID_FA20Pre.csv")

# analysis top three things for help------

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

wordcloud(words = d$word,freq = d$freq,min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))




