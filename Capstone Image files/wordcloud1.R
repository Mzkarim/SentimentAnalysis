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
library(wordcloud)
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

#tweets_geolocated = searchTwitter(' #AgeOfUltron',n=200,since="2015-01-01",lang="en",geocode="34.049933,-118.240843,50mi")
#tweets_geolocated.df <- twListToDF(tweets_geolocated)

tweets = searchTwitter('#Ironman3',n=200, lang="en")
tweets


#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#some_txt = sapply(tweets_geolocated, function(x) x$getText())

#tweet_txt = sapply( unlist(tweets_geolocated) , function(x) '$'( x , "text"))

some_txt = sapply(tweets, function(x) x$getText())

tweet_txt = sapply( unlist(tweets) , function(x) '$'( x , "text"))



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

# convert to dataframe
sentiment <- data.frame(text=some_txt, stringsAsFactors=FALSE)
write.csv(sentiment,"c:/Sentiment Analysis/Ironman3dataset.csv")


library(tm)

# Collect data
tweets.Ironman3<-read.csv('c:/Sentiment Analysis/Ironman3dataset.csv',header=T)


tweets.Ironman3["class"]<-rep("App",nrow(tweets.Ironman3))


# Helper Function
replacePunctuation <- function(x)
{
  x <- tolower(x)
  x <- gsub("[.]+[ ]"," ",x)
  x <- gsub("[:]+[ ]"," ",x)
  x <- gsub("[?]"," ",x)
  x <- gsub("[!]"," ",x)
  x <- gsub("[;]"," ",x)
  x <- gsub("[,]"," ",x)
  x
}

# Do our punctuation stuff
tweets.Ironman3$Tweet <- replacePunctuation(tweets.Ironman3$Tweet)


# Create corpus
tweets.Ironman3.corpus <- Corpus(VectorSource(as.vector(tweets.Ironman3$Tweet)))


# Create term document matrix
tweets.Ironman3.matrix <- t(TermDocumentMatrix(tweets.Ironman3.corpus,control = list(wordLengths=c(4,Inf))));

# Probability Matrix
probabilityMatrix <-function(docMatrix)
{
  # Sum up the term frequencies
  termSums<-cbind(colnames(as.matrix(docMatrix)),as.numeric(colSums(as.matrix(docMatrix))))
  # Add one
  termSums<-cbind(termSums,as.numeric(termSums[,2])+1)
  # Calculate the probabilties
  termSums<-cbind(termSums,(as.numeric(termSums[,3])/sum(as.numeric(termSums[,3]))))
  # Calculate the natural log of the probabilities
  termSums<-cbind(termSums,log(as.numeric(termSums[,4])))
  # Add pretty names to the columns
  colnames(termSums)<-c("term","count","additive","probability","lnProbability")
  termSums
}

tweets.Ironman3.pMatrix<-probabilityMatrix(tweets.Ironman3.matrix)



#Predict

# Get the test matrix
# Get words in the first document

getProbability <- function(testChars,probabilityMatrix)
{
  charactersFound<-probabilityMatrix[probabilityMatrix[,1] %in% testChars,"term"]
  # Count how many words were not found in the mandrill matrix
  charactersNotFound<-length(testChars)-length(charactersFound)
  # Add the normalized probabilities for the words founds together
  charactersFoundSum<-sum(as.numeric(probabilityMatrix[probabilityMatrix[,1] %in% testChars,"lnProbability"]))
  # We use ln(1/total smoothed words) for words not found
  charactersNotFoundSum<-charactersNotFound*log(1/sum(as.numeric(probabilityMatrix[,"additive"])))
  #This is our probability
  prob<-charactersFoundSum+charactersNotFoundSum 
  prob
}

# Get the matrix
tweets.test.matrix<-as.matrix(tweets.test.matrix)

# A holder for classification 
classified<-NULL

for(documentNumber in 1:nrow(tweets.test.matrix))
{
  # Extract the test words
  tweets.test.chars<-names(tweets.test.matrix[documentNumber,tweets.test.matrix[documentNumber,] %in% 1])
  # Get the probabilities
  mandrillProbability <- getProbability(tweets.test.chars,tweets.mandrill.pMatrix)
  otherProbability <- getProbability(tweets.test.chars,tweets.other.pMatrix)
  # Add it to the classification list
  classified<-c(classified,ifelse(mandrillProbability>otherProbability,"App","Other"))
}

View(cbind(classified,tweets.test$Tweet))