
#####################################################################

# STEP 1: INSTALL REQUIRED PACKAGES AND CONNECT TO ALL LIBARIES

#####################################################################

install.packages("devtools")
require(devtools)
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)
ls("package:sentiment")


# install_url("http://cran.r-project.org/src/contrib/Archive/Snowball/Snowball_0.0-11.tar.gz")
# require(snowball)

library(RCurl)
library(ROAuth)
library(twitteR)
library(plyr)
library(ggplot2)
library(reshape2)
library(sentiment)
library(wordcloud)
library(RColorBrewer)
#####################################################################

# STEP 2: SET UP TWITTER API TO WORK ON R ENVIRONMENT

#####################################################################

#library(RCurl)
#library(ROAuth)
#library(twitteR)


download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#Set constant request URL
requestURL <- "https://api.twitter.com/oauth/request_token"

# Set constant access URL
accessURL <- "https://api.twitter.com/oauth/access_token"

# Set constant auth URL
authURL <- "https://api.twitter.com/oauth/authorize"


# Put the both Consumer Key and Consumer Secret key from Twitter App.
consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE"  
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"


#Create the authorization object by calling function OAuthFactory
Cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL=requestURL,
                         accessURL=accessURL, 
                         authURL=authURL)

# Handshake Oauth; Get code and enter it on URL in Console
Cred$handshake(
  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))


#OAUTH Authentication

consumerKey <- "59oDfXxmBBm22p2j3Gowy4lEE" 
consumerSecret <- "bZufUMPivqtX94xG4Bt3QmsmqyL7TsDbkW8Kuo3cGYeFfKoysY"
access_Token <- "3060838521-u5eXreDFHOqaxUcvTYMFyuEXImu5RlpdiY436h8" 
access_Secret <- "Q55FxITLmzlJWW4xpNbwnsW2UPXQZL4KiOWf9QdsDlYKt"

# Create Twitter connection
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)



#####################################################################

# STEP 3 : COLLECT TWEETS 

#####################################################################

#Use the searchTwitter function to only get tweets 


tweets = searchTwitter('#Ironman3',n=200, lang="en")
tweets


#####################################################################

# STEP 4 : PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################



tweet_txt = sapply(tweets, function(x) x$getText())

# 4.1 : convert to dataframe

sentiment <- data.frame(text=some_txt, stringsAsFactors=FALSE)
write.csv(sentiment,"c:/Sentiment Analysis/Ironmandataset.csv")
write.table(tweet_txt, "c:/Sentiment Analysis/Ironman3.txt", sep="\t")

# Cleaning data

## STEP 4.2: Create clean function to preprocess the content  
clean.text <- function(some_txt)
{  
  ### STEP 4.2(a): Remove punctualtion, links
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  ## STEP 4.2(b): Remove non-english characters
  some_txt = gsub("[^\x20-\x7E]", "", some_txt)
  
  # STEP 4.2(c) : Define "tolower error handling" function
  try.tolower = function(x)
  {  
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

### STEP 4.2(d): Clean the content with "clean.text" function & remove qoute
tweet_txt = clean.text(tweet_txt)

## STEP 4.2(e) : Save a copy of clean data 
write.table(tweet_txt, "c:/Sentiment Analysis/ironmAN.txt", sep="\t")

#####################################################################

# STEP 5: PERFORM SENTIMENT ANALYSIS OF TWEETS 
# (BY POLARITY CATEGORIES: Method- Learning Based)

#####################################################################

# STEP 5.1  : classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")

# STEP 5.2 : get polarity best fit
polarity = class_pol[,4]


## STEP 5.3: Create data frame to obtain some general statistics

# data frame with results
sent_df = data.frame(text=tweet_txt,polarity=polarity, stringsAsFactors=FALSE)


### STEP 5.4: Perform data visualization
ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + 
  xlab("Polarity Categories") + ylab("Number of Tweets") + 
  ggtitle("Sentiment Analysis of Tweets on Twitter About Ironman3 Movie\n(Classification by Polarity)")


#####################################################################

# STEP 6: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#To extract the text and save it 
tweets.text=laply(tweets,function(t)t$getText())



#####################################################################

# STEP 7: ESTIMATE TWEET'S SENTIMENT  
# (BY PERFORMING "SENTIMENT SCORING ALGORITHM": Method 2- Lexicon Based)
# (Following Breen's Approach)

#####################################################################

## STEP 7.1: Create function to score on word from text based on negative and postive

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
    
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df <- data.frame(score=scores, text=sentences)
  
  return(scores.df)
}

### STEP 7.2(a): Load folder with positive  & negative dictionary in R

## folder with positive dictionary
pos <- scan('C:/Sentiment Analysis/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')

## folder with negative dictionary
neg <- scan('C:/Sentiment Analysis/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

### STEP 7.2(b): Add words to list
pos.words = c(pos, 'upgrade')
neg.words = c(neg, 'wtf', 'wait', 'waiting', 'epicfail')


##################################################
## STEP 8 : Final Analysis : Analysis the Score
#################################################

#Our first sentiment analysis result

scores<-score.sentiment(tweets$text, pos.words, neg.words,.progress='text')

table=tweets$scores

analysis=score.sentiment(tweet_txt,pos,neg,.progress='text')
table(analysis$score)
hist(analysis$score)


