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
#library(wordcloud)
library(sentiment)


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
consumerKey <- "1D7xYJ2yXqVbqMKg2HouxPaGo" 
consumerSecret <- "4jn2ZIvTXAOkNXmHB7M9GFRmdy1z6bJ2zOFjFYnxoOPA7qxhoy" 


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

consumerKey <- "1D7xYJ2yXqVbqMKg2HouxPaGo" 
consumerSecret <- "4jn2ZIvTXAOkNXmHB7M9GFRmdy1z6bJ2zOFjFYnxoOPA7qxhoy"
access_Token <- "3060838521-AyKiYweJgEeW3xJCV27NBfB1x9Mm5pK8GaUIOrL" 
access_Secret <- "1bnN7L9Ng0JokFmeCuJjfhxULL7z3aYxQJoutMskwy6qW"

# Create Twitter connection
setup_twitter_oauth(consumerKey,consumerSecret,access_Token,access_Secret)


#####################################################################

# STEP 4: COLLECT TWEETS 

#####################################################################

#Use the searchTwitter function to only get tweets within 50 miles of Los Angeles

3tweets_geolocated = searchTwitter(' #AgeOfUltron',n=200,since="2015-01-01",lang="en",geocode="34.049933,-118.240843,50mi")
#tweets_geolocated.df <- twListToDF(tweets_geolocated)

tweets = searchTwitter('#Ironman3',n=100, lang="en")
tweets


#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#some_txt = sapply(tweets_geolocated, function(x) x$getText())

#tweet_txt = sapply( unlist(tweets_geolocated) , function(x) '$'( x , "text"))

some_txt = sapply(tweets, function(x) x$getText())

tweet_txt = sapply( unlist(tweets) , function(x) '$'( x , "text"))


# convert to dataframe
sentiment <- data.frame(text=some_txt, stringsAsFactors=FALSE)
write.csv(sentiment,"c:/Sentiment Analysis/tweetdataset2.csv")

## How many unique tweets of each keyword
length(tweet_txt)

## remove retweet entities
tweet_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_txt) 
## remove at people
tweet_txt = gsub("@\\w+", "", tweet_txt) 

## remove punctuation
tweet_txt = gsub("[[:punct:]]", "", tweet_txt) 

## remove numbers
tweet_txt = gsub("[[:digit:]]", "", tweet_txt) 

## remove html links
tweet_txt = gsub("http\\w+", "", tweet_txt) 

# Remove non-english characters
tweet_txt = gsub("[^\x20-\x7E]", "", tweet_txt)

## remove unnecessary spaces
tweet_txt = gsub("[ \t]{2,}", "", tweet_txt)
tweet_txt= gsub("^\\s+|\\s+$", "", tweet_txt)

## define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

## lower case using try.error with sapply 
tweet_txt = sapply(tweet_txt, try.error)

#####################################################################

# STEP 6: PERFORM SENTIMENT ANALYSIS OF TWEETS 
# (BY POLARITY CATEGORIES: Method- Learning Based)

#####################################################################

## classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")

## get polarity best fit
polarity = class_pol[,4]

# data frame with results

tweet_sentiment <- data.frame(text=tweet_txt,polarity=polarity, stringsAsFactors=FALSE)

#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#To extract the text and save it 
tweets.text=laply(tweets,function(t)t$getText())

#folder with positive dictionary
pos <- scan('C:/Sentiment Analysis/opinion-lexicon-english/positive-words.txt', what='character', comment.char=';')

#folder with negative dictionary
neg <- scan('C:/Sentiment Analysis/opinion-lexicon-english/negative-words.txt', what='character', comment.char=';')


#####################################################################

# STEP 8: ESTIMATE TWEET'S SENTIMENT  
# (BY PERFORMING "SENTIMENT SCORING ALGORITHM": Method 2- Lexicon Based)
# (Following Breen's Approach)

#####################################################################

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  
{
  #require(plyr)
  #require(stringr)
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

#############################################
       #Final Analysis
###########################################

#Our first sentiment analysis result

analysis=score.sentiment(tweets.text,pos,neg,.progress='text')
