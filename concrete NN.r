concrete <- read.csv("C:/Users/Suresh/Downloads/concrete.csv")
View(concrete)
str(concrete)
concrete_norm <- scale(concrete[,-9])
View(concrete_norm)
concrete_norm <-data.frame(concrete_norm,concrete$strength)
View(concrete_norm)
colnames(concrete_norm)[9] <- "strength"
View(concrete_norm)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]
library(neuralnet)
library(nnet)

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train, hidden = 1)
str(concrete_model)
plot(concrete_model)
set.seed(15000)
results <- compute(concrete_model,concrete_test[,1:8])
str(results)
pred_results <- results$net.result
cor(pred_results,concrete_test$strength)
plot(pred_results,concrete_test$strength)
concrete_model$covariate

ci <- confidence.interval(concrete_model,alpha = 0.05)
ci
model3<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 3)
plot(model3)
