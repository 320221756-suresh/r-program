sms_raw_NB <- read.csv("C:/Users/Suresh/Downloads/sms_raw_NB.csv")
View(sms_raw_NB)
sms <- sms_raw_NB
str(sms)
sms$type <- factor(sms$type)
str(sms$type)
table(sms$type)
library(tm)
sms_corpus <- Corpus(VectorSource(sms$text))
sms_corpus
inspect(sms_corpus[1:3])
clean <- tm_map(sms_corpus, content_transformer(tolower))
clean <- tm_map(clean,removeNumbers)
clean <- tm_map(clean,removeWords, stopwords())
clean <- tm_map(clean,removePunctuation)
clean <- tm_map(clean,stripWhitespace)
inspect(clean[1:3])
sms_doc <- DocumentTermMatrix(clean)
sms_doc

sms_train_raw <- sms[1:4447,]
sms_test_raw <- sms[4448:5559,]

sms_doctrain <- sms_doc[1:4447,]
sms_doctest <- sms_doc[4448:5559,]

sms_cleantrain <- clean[1:4447]
sms_cleantest <- clean[4448:5559]
table(sms_train_raw$type)
table(sms_test_raw$type)

sms_doc<-findFreqTerms(sms_doctrain, 5)
sms_train <- DocumentTermMatrix(sms_cleantrain, list(dictionary = sms_doc))
sms_test  <- DocumentTermMatrix(sms_cleantest, list(dictionary = sms_doc))


convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

library(e1071)
model <- naiveBayes(sms_train,sms_train_raw$type)
summary(model)
model

sms_test_pred <- predict(model,sms_test)
library(gmodels)
CrossTable(sms_test_pred,sms_test_raw$type,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,dnn = c('predicted', 'actual'))
mean(sms_test_pred==sms_test_raw$type)
