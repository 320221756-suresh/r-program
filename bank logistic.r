bank<- read.csv("C:/Users/Suresh/Downloads/bank-full (1).csv", sep=";")
View(bank)
str(bank)
bank$job <- as.numeric(bank$job)
bank$marital <- as.numeric(bank$marital)
bank$education <- as.numeric(bank$education)
bank$default <- as.numeric(bank$default)
bank$housing <- as.numeric(bank$housing)
bank$loan <- as.numeric(bank$loan)
bank$contact <- as.numeric(bank$contact)
bank$month <- as.numeric(bank$month)
bank$poutcome <- as.numeric(bank$poutcome)
str(bank)
summary(bank)
bank1 <- scale(bank[,-17])
bank1 <- data.frame(bank1,bank$y)
View(bank1)
dim(bank1)
attach(bank1)

model <- lm(bank.y~., data = bank1)
model
pred <- predict(model,bank1)
pred
plot(bank1$bank.y, pred)
model2 <- glm(bank.y~.,data=bank1,family = "binomial")

exp(coef(model2))
prob <- predict(model2,bank1,type="response")
summary(model2)
confusion<-table(prob>0.5,bank1$bank.y)
confusion
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
#roc
library(ROCR)
rocrpred<-prediction(prob,bank1$bank.y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))

