Fraud_check <- read.csv("C:/Users/Suresh/Downloads/Fraud_check.csv")
fraud <-(Fraud_check[,-c(1,2,6)])
str(fraud)

yes_no <- ifelse(fraud$Taxable.Income<=30000,"good","Riskey")
fraud[,"yes_no"] <- yes_no
fraud <- data.frame(scale(fraud[,1:3]),fraud[,4])

View(fraud)

fraud_riskey <- fraud[fraud$fraud...4.=="Riskey",]
fraud_good <- fraud[fraud$fraud...4.=="good",]
fraud_train <- rbind(fraud_riskey[1:99,],fraud_good[1:99,])
fraud_test <- rbind(fraud_riskey[100:124,],fraud_good[100:124,])
attach(fraud)
library(tree)
library(party)
formula <- (fraud...4.~Taxable.Income+Work.Experience+City.Population)
fraud_tree <- ctree(formula, data = fraud_train)
summary(fraud_tree)

plot(fraud_tree)

pred_fraud <- predict(fraud_tree,fraud_train)
View(pred_fraud)
library(caret)
confusionMatrix(pred_fraud,fraud_train$fraud...4.)
fraud_tttest <- predict(fraud_tree,fraud_test)
confusionMatrix(fraud_tttest,fraud_test$fraud...4.)
library(gmodels)
CrossTable(fraud_train$fraud...4.,pred_fraud)
