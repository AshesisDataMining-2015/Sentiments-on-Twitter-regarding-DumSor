
#loading required R libraries
library(twitteR)
library(ROAuth)
library(RCurl)
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("sentiment")

#loading required packages
library(twitteR)
library(wordcloud)
library(plyr)
library(ggplot2)
library(sentiment)

#Find OAuth setting for twitter:
library(httr)
oauth_endpoints("twitter")
api_key <- "iBsFRCRDL42ybUxme4FsXWQ0J"
api_secret <- "I9aMpqb0Eld9H8Ts9o5ZBJuuxfO44m9TNIZSyDLYlTsEUxDdVz"
access_token <- "488264531-Na2z1QWU1NaAKSlpskFTkYmMHxMrFGLSxcmMkMlm"
access_token_secret <- "xBOBLOSzfX7OY3iOvhzrW9270V8bdCG6uXbCHy9zr3cjs"

#Getting 2000 tweets about ecg
ecg_tweets = searchTwitter("ECG", n=2000, lang="en")
ecg_tweets = searchTwitter(searchString = "ECG", n = 2000, lang = "en")
#Getting the text from the tweets
ecg_txt = sapply(ecg_tweets, function(x) x$getText())
#Remove RTs from the tweets got
ecg_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","", ecg_txt)
#Remove all mentions "@person"
ecg_txt = gsub("@\\w+", "", ecg_txt)
ecg_txt = gsub("[[:punct:]]", "", ecg_txt)
#^^Removing punctuation
#Removing numbers
ecg_txt = gsub("[[:digit:]]", "", ecg_txt)
#Removing html links
ecg_txt = gsub("http\\w+", "", ecg_txt)
#unnecessary spaces
ecg_txt = gsub("[\t]{2,}", "", ecg_txt)
ecg_txt = gsub("^\\s+|\\s+$", "", ecg_txt)
catch.error = function(x)
{
  y = NA
  catch_error = try(Catch(tolower(x), error=function(e) e)
  if(!inherits(catch_error, "error"))
  y = tolower(x)
  return(y)
}
ecg_txt = sapply(ecg_txt, catch.error)


