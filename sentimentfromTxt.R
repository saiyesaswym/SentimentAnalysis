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

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")   

#build vector with matches between sentence and each category
vPosmatch <- match(words, vPosTerms)
posmatch <- match(words, posTerms)
vNegmatch <- match(words, vNegTerms)
negmatch <- match(words, negTerms)

#finding the positions of words in the main list
vPosnum <- vPosmatch[!is.na(vPosmatch)]
posnum <- posmatch[!is.na(posmatch)]
vNegnum <- vNegmatch[!is.na(vNegmatch)]
negnum <- negmatch[!is.na(negmatch)]

#separating the words based on their positions
#Getting the four different categories of words
vPosWords <- vPosTerms[c(vPosnum)]
posWords <- posTerms[c(posnum)]
vNegWords <- vNegTerms[c(vNegnum)]
negWords <- negTerms[c(negnum)]

vPosCount <- sum(!is.na(vPosmatch))
posCount <- sum(!is.na(posmatch))
vNegCount <- sum(!is.na(vNegmatch))
negCount <- sum(!is.na(negmatch))

all_words <- c(vPosWords,posWords,vNegWords,negWords)

sentiment <- c("Very Positive","Positive","Very Negative","Negative")
count <- c(vPosCount,posCount,vNegCount,negCount)
words_count <- data.frame(sentiment,count)


#building wordcloud
wordcloud(words, scale=c(6,0.7), max.words=150,random.order=FALSE, rot.per=0.35,colors=brewer.pal(8,"Dark2"))

#Build wordclouds for different categories
col=brewer.pal(6,"Dark2")
wordcloud_Neg <- wordcloud(negWords,min.freq = 2,max.words = 20,random.color = T,colors = col)
wordcloud_VNeg <- wordcloud(vNegWords,min.freq = 2,max.words = 20,random.color = T,colors = col)
wordcloud_Pos <- wordcloud(posWords,min.freq = 2,max.words = 20,random.color = T,colors = col)
wordcloud_VPos <- wordcloud(vPosWords,min.freq = 2,max.words = 20,random.color = T,colors = col)

wordcloud_all <- wordcloud(all_words,min.freq = 5,max.words = 30,random.color = T,colors = col)

#plotting the sentiment count
wordcount_plot <- ggplot(words_count,aes(x=sentiment,y=count,color=sentiment,fill=sentiment))+geom_bar(stat = 'identity')+
  geom_text(aes(label=count), position=position_dodge(width=0.9), vjust=-0.25)+theme_minimal()


sentimentscore(reviews)
