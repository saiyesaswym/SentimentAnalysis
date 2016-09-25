#include required libraries
library(plyr)
library(tm)
library(twitteR)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

#get the data
reviews <- read.csv("textreviews.csv",stringsAsFactors = FALSE,header = FALSE)

#function to clean data
reviews_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",reviews)
reviews_cl = gsub("http[^[:blank:]]+", "", reviews_cl)
reviews_cl = gsub("@\\w+", "", reviews_cl)
reviews_cl = gsub("[ \t]{2,}", "", reviews_cl)
reviews_cl = gsub("^\\s+|\\s+$", "", reviews_cl)
reviews_cl = gsub("[[:punct:]]", " ", reviews_cl)
reviews_cl = gsub("[^[:alnum:]]", " ", reviews_cl)
reviews_cl <- gsub('\\d+', '', reviews_cl)
sentence <- tolower(reviews_cl)

sentence <- removeWords(sentence,c(stopwords("english"),letters))

#remove unnecessary characters and split up by word
wordList <- str_split(sentence, '\\s+')
words <- unlist(wordList)

#loading the dictionary
afinn_list <- read.delim(file='C:/Users/Teja/Documents/R workspace/affin.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

list1 <- match(words,afinn_list$word)

#finding the positions of words in the main list
num1 <- list1[!is.na(list1)]

words1 <- afinn_list[c(num1),]

words2 <- unique.array(words1)
