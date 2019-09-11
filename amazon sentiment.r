library(rvest)
library(XML)
library(magrittr)
aurl <- "https://www.amazon.in/Redmi-Pro-Black-64GB-Storage/product-reviews/B07DJHXWZZ/ref=dpx_acr_txt?showViewpoints=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"redmi.txt",row.names = F)
getwd()
redmi <- read.csv2("~/redmi.txt", sep="")
View(redmi)
library(tm)
corpus <- iconv(redmi$x,to ="UTF-8")
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

redmi1 <- read.csv2("~/redmi.txt", sep="")
corpus1 <- iconv(redmi1$x,to ="UTF-8")

z <- get_nrc_sentiment(corpus1)
head(z)
barplot(colSums(z), las = 2, col = "blue")
