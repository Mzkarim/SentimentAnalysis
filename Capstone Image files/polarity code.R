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
consumerKey <- "WOkhwRl4zaDBGYlcxOoOObxug"  
consumerSecret <- "7rWJayHoIGDHWo3xXY0vymlFqhaC2zZLblLZdywEXR7M5prj83"


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

#consumerKey <- "XHwzNgXjBk6hRkQvrvNPCuq8X" 
#consumerSecret <- "mrCqaBr0s3cZrC3pnfgsPp6CyRH2jcqPnkg5yeWvbKmvMRq0sY"
access_Token <- "3290178023-pQgqNOuINlN6w8ekh2VqyeOxnFnut9aKNlKHfuI" 
access_Secret <- "VFXNr2MZeIEl2UM1MmP1IkUuvbLFhVdHnpBEkkPlyn1fw"

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



# classify polarity
class_pol = classify_polarity(tweet_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]


## Visualization 
# plot distribution of polarity

ggplot(sent_df, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + 
  xlab("Polarity Categories") + ylab("Number of Tweets") + 
  ggtitle("Sentiment Analysis of Tweets on Twitter About Ironman3 Movie\n(Classification by Polarity)")




# data frame with results
#sent_df = data.frame(text=some_txt,polarity=polarity, stringsAsFactors=FALSE)


#folder with positive dictionary
pos <- scan('C:/Sentiment Analysis/opinion-lexicon-english/positive-words.txt', what='character', comment.char=';')

#folder with negative dictionary
neg <- scan('C:/Sentiment Analysis/opinion-lexicon-english/negative-words.txt', what='character', comment.char=';')











#####################################################################

# STEP 5: PREPARE THE TEXT FOR SENTIMENT ANALYSIS

#####################################################################

#To extract the text and save it 
tweets.text=laply(tweets,function(t)t$getText())

#folder with positive dictionary
pos <- scan('C:/Sentiment Analysis/positive-words.txt', what='character', comment.char=';')

#folder with negative dictionary
neg <- scan('C:/Sentiment Analysis/negative-words.txt', what='character', comment.char=';')


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
###########################no need
## STEP 9.1: Transforming Text

### STEP 9.1.1: Build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors

# library(tm)

df <- data.frame(V1 = tweets.txt, stringsAsFactors = FALSE)

mycorpus <- Corpus(VectorSource(Kejriwal_txt))
##########################no need

tweets.df <- twListToDF(tweets)
dim(tweets.df)

library(tm)
## Loading required package: NLP
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(tweets.df$text))
# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
### myCorpus <- tm_map(myCorpus, removeURL, lazy=TRUE) 
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??
# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
#???# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first 5 documents (tweets) inspect(myCorpus[1:5]) 
# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}

myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), dictionary = myCorpusCopy, lazy=TRUE)

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm

idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx + (0:5), 101:110])

#  mycorpus <- Corpus(VectorSource(Modi_txt))
#    mycorpus <- Corpus(VectorSource(Rahul_txt))

### STEP 9.1.2: Preprocessing Text
# (Changing letters to lower case, removing punctuations/numbers & stop words) 

df <- data.frame(V1 = tweet_txt, stringsAsFactors = FALSE)

mycorpus <- Corpus(VectorSource(tweet_txt))


clean.corpus<-function(mycorpus)
{
  mycorpus = tm_map(mycorpus, removePunctuation)
  mycorpus = tm_map(mycorpus, stripWhitespace)
  mycorpus = tm_map(mycorpus, content_transformer(tolower))
  
  # The general English stop-word list is tailored by adding 
  # "available" and "via" and removing "r")
  myStopwords <- c(stopwords('english'), "available", "via")
  idx <- which(myStopwords == "r")
  myStopwords <- myStopwords[-idx]
  mycorpus <- tm_map(mycorpus, removeWords, myStopwords)
  
  return(mycorpus)
}

tweet.corpus = clean.corpus(mycorpus)




## STEP 9.3: Stemming words to retrieve the root form, so that words look normal

# require(snowball)
# require(Rweka)
# require(rJava)
# require(Rwekajars)

### STEP 9.3.1: Perform stemDocument
### STEP 9.3.2: Perform stemDocument
### STEP 9.3.3: Inspect the first three "documents"
### STEP 9.3.4: Stem completion
### STEP 9.3.5: Print the first three documents in the built corpus
## STEP 9.4: Build a Document-Term Matrix
## STEP 9.5: Build a td Matrix from a corpus & find list of words associated wth "Kejriwal"

#-------- Kejriwal    

dictcorpus <- tweet.corpus

tweet.corpus <- tm_map(tweet.corpus, stemDocument)
inspect(tweet.corpus[4])
tweet.corpus.copy<-tweet.corpus

tweet.corpus.copy[[4]]

tweetcorpus.fun<-function(corpus, lower, upper)
{
  for(i in lower:upper)
  {
    cat(paste("[[", i, "]]", sep=""))
    writeLines(strwrap(corpus[[i]], width=73))
    
    cat("\n")
  }
}

tweetcorpus.fun(tweet.corpus,4,9)


mycorpus <- tm_map(tweet.corpus, stemCompletion, dictionary=dictcorpus)
inspect(tweet.corpus[1:3])

myDtm <- TermDocumentMatrix(tweet.corpus, control = list(minWordLength = 1))
inspect(myDtm[100:105,10:13])

findFreqTerms(myDtm, lowfreq=30)
findAssocs(myDtm, 'Kejriwal', 0.80)



#####################################################################
#                                                                   #
#                             END                                   #
#                                                                   #        
#####################################################################


#mycorpus = tm_map(mycorpus, tolower)
#mycorpus = tm_map(mycorpus, removePunctuation)
#some_stopwords = c(stopwords('english'))
#mycorpus = tm_map(mycorpus, removeWords, some_stopwords)
#mycorpus = tm_map(mycorpus, removeNumbers)

# Remove non-english characters
#mycorpus = gsub("[^\x20-\x7E]", "my.corpus")

#some_stopwords <- c(stopwords('english'))
#mycorpus = tm_map(mycorpus, removeWords, some_stopwords)



Text <- readLines(n=4)
df <- data.frame(V1 = Text, stringsAsFactors = FALSE)

mycorpus <- Corpus(DataframeSource(df))
tdm <- TermDocumentMatrix(mycorpus, control = list(removePunctuation = TRUE, stopwords = TRUE))

#  dictcorpus <- mycorpus
#    mycorpus <- tm_map(mycorpus, stemDocument)
#inspect(mycorpus[1:3])
# mycorpus <- tm_map(mycorpus, stemCompletion, dictionary=dictcorpus)
#inspect(mycorpus[1:3])

#write.csv(Kejriwal_sentiment, file="Kejriwal_sentiment.csv")
#View(kejriwal_sentiment)


text <- c("My name is naimul", "Flight was delayed")
words <- strsplit(text, " ")
