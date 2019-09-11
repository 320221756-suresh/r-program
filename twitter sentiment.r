Tweets <- read.csv("G:/classes/assignments/Tweets.csv")
View(Tweets)
str(Tweets)

library(tm)
corpus <- iconv(Tweets$text,to ="UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:3])
clean <- tm_map(corpus, content_transformer(tolower))
clean <- tm_map(clean, removePunctuation)
clean <- tm_map(clean,removeNumbers)
clean <- tm_map(clean, removeWords, stopwords("english"))
inspect(clean[1:3])
removeurl <- function(x) gsub('http[[:alnum:]]*','',x)
clean <- tm_map(clean, content_transformer(removeurl))
inspect(clean[1:3])
clean <- tm_map(clean, stripWhitespace)

tdoc <- TermDocumentMatrix(clean)
tdoc
tdoc <- as.matrix(tdoc)
tdoc

nrow(tdoc)
w <- rowSums(tdoc)
w
w <- subset(w, w>=10)
barplot(w, las =2, col = "red")
library(wordcloud)

wordcloud(words = names(w), freq = w)
#####

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

Tweets1 <- read.csv("G:/classes/assignments/Tweets.csv")
corpus1 <- iconv(Tweets1$text,to ="UTF-8")

z <- get_nrc_sentiment(corpus1)
head(z)
barplot(colSums(z), las = 2, col = "blue")
