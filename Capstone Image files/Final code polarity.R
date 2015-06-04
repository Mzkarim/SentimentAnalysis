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

# STEP 4: COLLECT TWEETS 

#####################################################################

#Use the searchTwitter function to only get tweets within 50 miles of Los Angeles

#tweets_geolocated = searchTwitter(' #AgeOfUltron',n=200,since="2015-01-01",lang="en",geocode="34.049933,-118.240843,50mi")
#tweets_geolocated.df <- twListToDF(tweets_geolocated)

tweets = searchTwitter('#Ironman3',n=200, lang="en")
tweets


#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#some_txt = sapply(tweets_geolocated, function(x) x$getText())

#tweet_txt = sapply( unlist(tweets_geolocated) , function(x) '$'( x , "text"))

tweet_txt = sapply(tweets, function(x) x$getText())

#tweet_txt = sapply( unlist(tweets) , function(x) '$'( x , "text"))


# convert to dataframe
#sentiment <- data.frame(text=some_txt, stringsAsFactors=FALSE)
#write.csv(sentiment,"c:/Sentiment Analysis/Ironmandataset.csv")

clean.text <- function(some_txt)
{  
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  
  ## STEP 5.3.1: Remove non-english characters
  some_txt = gsub("[^\x20-\x7E]", "", some_txt)
  
  # STEP 5.3.2: Define "tolower error handling" function
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


tweet_txt = clean.text(tweet_txt)

write.table(tweet_txt, "D:/Users/Lucky/Documents/ironmAN.txt", sep="\t")

# classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


## Visualization 
# plot distribution of polarity

# data frame with results
sent_df = data.frame(text=tweet_txt,polarity=polarity, stringsAsFactors=FALSE)

ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + 
  xlab("Polarity Categories") + ylab("Number of Tweets") + 
  ggtitle("Sentiment Analysis of Tweets on Twitter About Ironman3 Movie\n(Classification by Polarity)")







#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#To extract the text and save it 
tweets.text=laply(tweets,function(t)t$getText())

#folder with positive dictionary
pos <- scan('C:/Capstone Project/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')

#folder with negative dictionary
neg <- scan('C:/Capstone Project/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

#####################################################################

# STEP 8: ESTIMATE TWEET'S SENTIMENT  
# (BY PERFORMING "SENTIMENT SCORING ALGORITHM": Method 2- Lexicon Based)
# (Following Breen's Approach)

#####################################################################

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

pos.words = c(pos, 'upgrade')
neg.words = c(neg, 'wtf', 'wait', 'waiting', 'epicfail')


#############################################
#Final Analysis
###########################################

#Our first sentiment analysis result

scores<-score.sentiment(tweets$text, pos.words, neg.words,.progress='text')

table=tweets$scores

analysis=score.sentiment(tweet_txt,pos,neg,.progress='text')
table(analysis$score)
hist(analysis$score)

