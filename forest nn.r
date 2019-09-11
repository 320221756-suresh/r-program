forestfires <- read.csv("C:/Users/Suresh/Downloads/forestfires.csv")
View(forestfires)
str(forestfires)
forest_norm <-as.data.frame(scale(forestfires[,-c(1,2,31)]))

forest_train <- forest_norm[1:410,]
forest_test <- forest_norm[411:517,]
library(neuralnet)
library(nnet)
attach(forest_norm)

model <-neuralnet(area~dayfri+daymon+daysat+daysun+daythu+daytue+daywed+DC+DMC+FFMC+ISI+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep+rain+RH+temp+wind,data = forest_train, hidden = 6,stepmax=1e6)
str(model)
plot(model)

set.seed(1000)
model_results <- compute(model,forest_test[,-9])
str(model_results)
pred_area <- model_results$net.result
cor(pred_area,forest_test$area)
plot(pred_area,forest_test$area)

model_q <-neuralnet(area~dayfri+daymon+daysat+daysun+daythu+daytue+daywed+DC+DMC+FFMC+ISI+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthnov+monthoct+monthsep+rain+RH+temp+wind,data = forest_norm, hidden = 10,stepmax=1e6)
str(model_q)
plot(model_q)

set.seed(1000)
model_results <- compute(model_q,forest_test[,-9])
str(model_results)
pred_area <- model_results$net.result
cor(pred_area,forest_test$area)
plot(pred_area,forest_test$area)
