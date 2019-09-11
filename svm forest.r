forestfires <- read.csv("C:/Users/Suresh/Downloads/forestfires.csv")
View(forestfires)
forest <- forestfires
str(forest)
forest$day <- as.numeric(forest$day)
forest$month <- as.numeric(forest$month)
View(forest)
forest1 <- scale(forest[,-31])
forest1 <- data.frame(forest1,forest$size_category)
summary(forest1)
View(forest1)
str(forest1)
forest_train <- forest1[1:360,]
forest_test <- forest1[361:517,]
library(kernlab)
library(caret)


model <- ksvm(forest.size_category~., data = forest_train,kernel = "vanilladot")
summary(model)
model

pred <- predict(model, forest_test)
mean(pred==forest_test$forest.size_category)


model1 <- ksvm(forest.size_category~.,data = forest_train,kernel = "rbfdot")
summary(model1)
model1

pred <- predict(model1, forest_test)
mean(pred==forest_test$forest.size_category)

