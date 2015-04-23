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

#Setting Up Twitter oAuth
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
#Getting 2000 tweets about dumsor
ecg_tweets = searchTwitter(searchString = "dumsor", n = 2000, lang = "en")
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
catch.error = function(x)
{
  y = NA
  catch_error = tryCatch(tolower(x), error=function(e)e)
  if(!inherits(catch_error, "error"))
  y = tolower(x)
  return(y)
}
ecg_txt = sapply(ecg_txt, catch.error)
ecg_txt = ecg_txt[!is.na(ecg_txt)]
names(ecg_txt) = NULL
ecg_class_emo = classify_emotion(ecg_txt, algorithm = "bayes", prior=1.0)
emotion = ecg_class_emo[,7]
emotion[is.na(emotion)] = "unknown"
ecg_class_pol = classify_polarity(ecg_txt, algorithm = "bayes")
polarity = ecg_class_pol[,4]
sentiment_dataframe = data.frame(text=ecg_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
sentiment_dataframe = within(sentiment_dataframe, emotion <- factor(emotion, levels=names(sort(table(emotion),decreasing=TRUE))))
#Emotions
ggplot(sentiment_dataframe, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) + scale_fill_brewer(palette="Dark2") + ggtitle("Sentiment Analysis of Tweets on Twitter about DumSor") +theme(legend.position="right") + ylab("Number of Tweets") + xlab("Emotion Categories")
#Polarity
ggplot(sentiment_dataframe, aes(x=polarity)) + geom_bar(aes(y=..count.., fill=polarity)) + scale_fill_brewer(palette="RdGy") + ggtitle("Sentiment Analysis of Tweets on Twitter about DumSor") + theme(legend.position="right") + ylab("Number of Tweets") + xlab("Polarity Categories")
#Separate words according to emotions
ecg_emos = levels(factor(sentiment_dataframe$emotion))
n_ecg_emos = length(ecg_emos)
ecg.emo.docs = rep("", n_ecg_emos)
for(i in 1:n_ecg_emos)
{
  tmp = ecg_txt[emotion == ecg_emos[i]]
  ecg.emo.docs[i] = paste(tmp, collapse = "")
}

#create corpus
ecg.corpus = Corpus(VectorSource(ecg.emo.docs))
ecg.tdm = TermDocumentMatrix(ecg.corpus)
ecg.tdm = as.matrix(ecg.tdm)
colnames(ecg.tdm) = ecg_emos
#creating, comparing and plotting the words on the cloud
comparison.cloud(ecg.tdm, colors = brewer.pal(n_ecg_emos, "Dark2"), scale=c(3,.5), random.order = FALSE, title.size = 1.5)
  






