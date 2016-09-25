
#' 
#' score.sentiment() implements a very simple algorithm to estimate
#' sentiment, assigning a integer score by subtracting the number 
#' of occurrences of negative words from that of positive words.
#' 
#' @param sentences vector of text to score
#' @param pos.words vector of words of postive sentiment
#' @param neg.words vector of words of negative sentiment
#' @param .progress passed to <code>laply()</code> to control of progress bar.
#' @returnType data.frame
#' @return data.frame of text and corresponding sentiment scores
#' @author Jefrey Breen <jbreen@cambridge.aero>
score.sentiment = function(sentences, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #loading the dictionary
  afinn_list <- read.delim(file='C:/Users/Teja/Documents/R workspace/affin.txt', header=FALSE, stringsAsFactors=FALSE)
  names(afinn_list) <- c('word', 'score')
  afinn_list$word <- tolower(afinn_list$word)
  
  neg.words <- c(afinn_list$word[afinn_list$score>=-5 & afinn_list$score<=-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
  pos.words <- c(afinn_list$word[afinn_list$score>=1 & afinn_list$score<=5], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
  
  score <- data.frame(senti.score=numeric(),stringsAsFactors = FALSE)
  pos.all <- data.frame(words=character(),stringsAsFactors = FALSE)
  neg.all <- data.frame(words=character(),stringsAsFactors = FALSE)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  j <- nrow(sentences)
  for (i in 1:j){
    sentence <- sentences[i,]
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
  
    posnum <- pos.matches[!is.na(pos.matches)]
    negnum <- neg.matches[!is.na(neg.matches)]
    
    posWords <- pos.words[c(posnum)]
    negWords <- neg.words[c(negnum)]
    
    words.all <- c(posWords,negWords)
    
    list1 <- match(words.all,afinn_list$word)
    
    #finding the positions of words in the main list
    num1 <- list1[!is.na(list1)]
    
    score1 <- sum(afinn_list[c(num1),2])
   
    score[i,] <- score1
  }
  
  scores.df = data.frame(score=score, text=sentences)
  return(scores.df)
}

