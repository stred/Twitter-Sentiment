
# R Twitter Sentiment Analysis by Stephen Redmond

# Based on code by Jeffrey Breen 
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107
# Copyright 2011 by Jeffrey Oliver Breen <jbreen@cambridge.aero>
# Used under Apache License 2.0
# http://www.apache.org/licenses/LICENSE-2.0



# STEP 1: If you haven't run this section ever, run it now

# Run this block only once per R installation
if (FALSE)
{
  # SETUP: This section installs the necessary packages
  # to get everything working.

  # In RStudio, select everything between --->  and <--- and hit Ctrl+R
  
  # --->
  install.packages("base64enc")
  install.packages("stringr")
  install.packages("plyr")
  install.packages("twitteR")
  # <---
  
}

# STEP 2: Run this section in a new R session

# Run once per session
if (FALSE)
{
  # This section only needs to be run once per session.
  # It creates the sentiment analysis function and opens
  # the session to Twitter.
  
  # In RStudio, select everything between --->  and <--- and hit Ctrl+R
  
  # --->

  # Path for results  ****** DON'T FORGET TO CHANGE THIS *****
  OutputPath = "c:\\Users\\username\\Downloads\\"
  
  # Load the core libraries needed by the process
  library(twitteR)
  library(base64enc)
  library(plyr)
  library(stringr)
  
  # This function compares the text to the bag of words and 
  # returns a score
  score.sentence <- function(sentence, pos.words, neg.words) {
    require(plyr)
    require(stringr)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl::]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }
  
  # Define the Twitter API Keys
  # Get these from https://apps.twitter.com/
  api_key <- 'your_consumer_key'
  api_secret <- 'your_consumer_secret'
  access_key <- 'your_access_token'
  access_token <- 'your_access_token_secret(despite_the_name)'
  
  # These are your "Bag of Words".
  # Example list from: https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
  # Direct Link: https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/data/opinion-lexicon-English/negative-words.txt
  # And: https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/blob/master/data/opinion-lexicon-English/positive-words.txt
  
  # Local Path to Dictionaries
  #BagOfPositiveWords = 'C:\\Users\\username\\Downloads\\lexicon\\positive-words.txt'
  #BagOfNegativeWords = 'C:\\Users\\username\\Downloads\\lexicon\\negative-words.txt'
  
  # Web based dictionary
  BagOfPositiveWords = 'https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/positive-words.txt'
  BagOfNegativeWords = 'https://raw.githubusercontent.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/master/data/opinion-lexicon-English/negative-words.txt'
  
  # Load the list of positive and negative words.
  hu.liu.pos = scan(BagOfPositiveWords, what='character', comment.char=';')
  hu.liu.neg = scan(BagOfNegativeWords, what='character', comment.char=';')

  # Connect to twitter using oAuth
  setup_twitter_oauth(api_key, api_secret, access_key, access_token)
  1
  # This "1" is just the response to store the credentials locally
  
  # <---
}

# STEP 3: Run this section as many times as you need to perform searches

# In RStudio, select everything between --->  and <--- and hit Ctrl+R

# --->

# Perform the required search
SearchTerm = "ncirl"
# Number of Tweets to retrieve - up to 1500
NumberOfTweets = 1500

# Set the Max ID to go back further in time, NULL means up to latest
tMaxID = NULL
#tMaxID = 668873412224290815
tweets=searchTwitteR(SearchTerm, NumberOfTweets, maxID = tMaxID)

# We can add our own poitive or negative words to the list
# Note that the sentiment 
pos.words = c(hu.liu.pos, 'open', 'pass','honours')
neg.words = c(hu.liu.neg, 'fail', 'dropout')

# Use laply to repeatedly call the sentence scoring function
# returning a data frame of the tweets with a score
tres<-laply(tweets, 
  function(t) {
    t2 <- t$toDataFrame()
    
    # Format the date for output
    t2$createdFormatted = as.character(t2$created, "%Y-%m-%d %H:%M:%S")  #as.POSIXct(t2$created, origin = "1970-01-01", tz="GMT")
    
    # Strip out non printable characters before processing
    t2$text = str_replace_all(t2$text, "[:^print:]", " ")
    
    # Note that we use a tryCatch here in case there 
    # is an error in the function - defaults to 0
    t2$score = tryCatch (
      score.sentence(t$text, pos.words, neg.words ),
      error=function(e) 
        0
      )
    return(t2)
  }
)

# Write the scored tweet table
WritePath <- paste(OutputPath, SearchTerm, tMaxID, "_output.tab", sep="")
write.table(tres, file = WritePath, append = FALSE, quote = TRUE, sep = "\t",
            fileEncoding = "UTF-8", eol = "\n", col.names = TRUE, row.names = FALSE)

# <---

