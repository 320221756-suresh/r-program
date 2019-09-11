`50_Startups.(1)` <- read.csv("C:/Users/Suresh/Downloads/50_Startups (1).csv")
View(`50_Startups.(1)`)
startup <- `50_Startups.(1)`
str(startup)
startup_norm <- as.data.frame(scale(startup[,c(-4)]))
View(startup_norm)

startup_train <- startup_norm[1:40,]
startup_test <- startup_norm[41:50,]
library(neuralnet)
library(nnet)
attributes(startup_norm)
attach(startup_norm)
startup_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup_norm,stepmax=1e6)
str(startup_model)
startup_model
plot(startup_model)
set.seed(1000)
result <- compute(startup_model,startup_test[,1:3])
str(result)
pred_results <- result$net.result
cor(pred_results,startup_test$Profit)
plot(pred_results,startup_test$Profit)
ci <- confidence.interval(startup_model, alpha = 0.05)
ci

startup_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup_train, hidden = 2)

plot(startup_model)
