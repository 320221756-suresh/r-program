library(rvest)
library(XML)
library(magrittr)
a<-10
rev<-NULL
url1<-"https://www.tripadvisor.in/Hotel_Review-g147399-d2354539-Reviews-or"
#url2<-"-The_Venetian_on_Grace_Bay-Providenciales_Turks_and_Caicos.html#REVIEWS"
for(i in 0:8){
  url<-read_html(as.character(paste(url1,i*a,sep="")))
  ping<-url %>%
    html_nodes(".partial_entry") %>%
    html_text() 
  rev<-c(rev,ping)
}
write.table(rev,"travel.txt")

travel <- read.table("~/travel.txt", header=TRUE, quote="\"")
View(travel)
library(tm)
corpus <- iconv(travel$x,to ="UTF-8")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:3])
clean <- tm_map(corpus, content_transformer(tolower))
clean <- tm_map(clean, removePunctuation)
clean <- tm_map(clean,removeNumbers)
clean <- tm_map(clean, removeWords, stopwords("english"))
inspect(clean[1:3])

clean <- tm_map(clean, stripWhitespace)

tdoc <- TermDocumentMatrix(clean)
tdoc
tdoc <- as.matrix(tdoc)
tdoc

nrow(tdoc)
w <- rowSums(tdoc)
w
w <- subset(w, w>=11)
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

travel1 <- read.table("~/travel.txt", header=TRUE, quote="\"")
corpus1 <- iconv(travel1$x,to ="UTF-8")

z <- get_nrc_sentiment(corpus1)
head(z)
barplot(colSums(z), las = 2, col = "blue")
