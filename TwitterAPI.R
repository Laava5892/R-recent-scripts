# Install and Activate Packages
install.packages("twitteR", "stringr")


install.packages(c("devtools", "rjson", "bit64", "httr"))

#RESTART R session!

library(devtools)
library(rjson)
library(bit64)
library(httr)
install_github("twitteR", username="geoffjentry/twitteR")

library(twitteR)
library(stringr)

# Declare Twitter API Credentials
api_key <- "GJLTxMvg3i1egzgt7KxLaZNcz" # From dev.twitter.com
api_secret <- "CpSP3fQp6szaP4EIheqDaDPcQvfhiwqviNxtTQAHbomhXspX8R" # From dev.twitter.com
token <- "2451724490-Dlz9e34nBGUW8TsLg6AVbihMHefH4wsgFPZaGVF" # From dev.twitter.com
token_secret <- "ThgKa95SSmyxYXCiCQkC3rYS3h4p022qQLIX9pIHKICM8" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).

tweets <- searchTwitter("#GameOfThrones", n=100, lang="en", since="2016-02-09", resultType = "recent")

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)

tweets_geolocated <- searchTwitter("#GameOfThrones", n=100, lang="en", geocode="34.049933,-118.240843,50mi", since="2016-02-01", resultType = "recent")
tweets_geolocated.df <- twListToDF(tweets_geolocated)

install.packages("tm")
library(tm)

View(tweets.df)

# tweets.df$text<-iconv(enc2utf8(tweets.df$text),sub="byte")

for (i in 1:100){
  unclean_tweet=tweets.df$text[i]
  clean_tweet = gsub("&amp", "", unclean_tweet)
  clean_tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet) 
  clean_tweet = gsub("@\\w+", "", clean_tweet)
  clean_tweet=gsub("@[a-z,A-Z]*", "", clean_tweet)
  clean_tweet=gsub("#[a-z,A-Z]*", "", clean_tweet)
  clean_tweet = gsub("[[:punct:]]", "", clean_tweet)
  clean_tweet = gsub("[[:digit:]]", "", clean_tweet)
  clean_tweet = gsub("http\\w+", "", clean_tweet)
  clean_tweet=gsub("#[a-z,A-Z]*", "", clean_tweet)
  clean_tweet = gsub("[ \t]{2,}", "", clean_tweet)
  clean_tweet = gsub("^\\s+|\\s+$", "", clean_tweet) 
  clean_tweet=gsub("[^0-9A-Za-z///' ]", "", clean_tweet)
  clean_tweet = tolower(clean_tweet)
  tweets.df$finaltext[i]=clean_tweet
}

# The tweets are first converted to a data frame and then to a corpus.

# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(tweets.df$finaltext))
writeLines(as.character(myCorpus[[1]]))
mystopwords <- c('a', 'the', 'of', 'at', 'to', 'from', 'in', 'for', 'is', 'are', 'on', 'that', 'it', 'and', 'game', 'thrones')
myCorpus <-tm_map(myCorpus, removeWords, mystopwords)
dtm <- DocumentTermMatrix(myCorpus)
freq <- colSums(as.matrix(dtm))
df <- data.frame(freq)
View(df)
write.csv(df, file = "twitter.csv")





