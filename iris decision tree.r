iris <- iris
str(iris)

iris_setosa<-iris[iris$Species=="setosa",] 
iris_versicolor <- iris[iris$Species=="versicolor",] 
iris_virginica <- iris[iris$Species=="virginica",] 
iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
install.packages('party')
library(party)
attach(iris)
formula <- (Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width)
iris_tree <- ctree(formula, data = iris_train)
plot(iris_tree)

table(predict(iris_tree),iris_train$Species)                    
#test
iris_tree_test <- ctree(formula, data = iris_test)
table(predict(iris_tree_test),iris_test$Species)
pred <- predict(iris_tree,iris_test)
View(pred)
mean(iris_test$Species==pred)
library(caret)
confusionMatrix(pred,iris_test$Species)
library(gmodels)
CrossTable(iris_test$Species,pred)

