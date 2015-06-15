# SentimentAnalysis
Sentiment Analysis on Twitter data

*****************************************************************************************************************************
*****************************************************************************************************************************
** For sentiment analysis We use statistical analysis package RStudio (Version 0.99.441). RStudio is a set of integrated ****
** tools designed to help us be more productive with R. It includes a console, syntax-highlighting editor that supports  ****
** direct code execution, as well as tools for plotting, history, debugging and workspace management. It can be          ****
** downloaded free from http://www.rstudio.com/products/rstudio/download/                                                ****
*****************************************************************************************************************************
*****************************************************************************************************************************

To do sentiment analysis with RStudio we first need to install necessary packages.

Our sentiment analysis code are divided into four parts/blocks :

    1. Collecting twitter data from twitter using twitter API  
          This is the R code used for twitter data collection using twitter API. Note that to extract twitter data through 
          twitter API we need to open an development account with twitter.The detail procedures along with twitter data
          collection process is shown in final report section.
          
    2. Sentiment analysis by Polarity Categories
          This is the code for doing sentiment analysis with polarity. Twitter data collected through twitter API contains
          unwanted symbols,numbers and many other characters that we do not need for calculating sentiment analysis. 
          Therefore we need to clean the data set first.

          Each word from individual tweet is compare with positive and negative words of a dictionary. If the first words is
          a positive one then it stored in pos otherwise in neg variable. the next positive or negative word is added with
          pos or neg counter .

          By Polarity Categories :( Method- Learning Based) we first classify the polarity ( positive,negative and neutral)
          then using ggplot2 we create a bar chart to represent the polarity categories. The highest  number of polarity
          represents the user sentiment about a particular subject.

    3. Sentiment analysis with Word Cloud (visualization) 
          This is the R code for making word cloud. Word cloud is a visual representation of text mining data. With text
          mining analysis we will search for  most frequently used word. The more the word used the more bigger size the 
          word appear in word cloud.
    
    4. Sentiment analysis with Text mining or text analytics approach ( tokenization)
          This R code is used for tokenization. We performed Unigram and Bigram tokenization to find out most frequently
          words used and most frequently bi gram used in tweeter data. The resultant word/words helps us to find what most
          of the people thinking about Ironman movie. Based on these words we can conclude a decision.
