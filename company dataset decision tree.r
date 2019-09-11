Company_Data <- read.csv("C:/Users/Suresh/Downloads/Company_Data.csv")
library(tree)
library(party)
str(Company_Data)
View(Company_Data)
comp_train <- Company_Data[2:320,]
comp_test <- Company_Data[321:400,]
variable.names(comp_test)
attach(Company_Data)
formula <- (ShelveLoc~CompPrice+Income+Advertising+Population+Price+Sales+Urban+Age+CompPrice)
comp_tree <- ctree(formula, data = comp_train)
plot(comp_tree)

text(comp_tree)

pred_train <- predict(comp_tree,comp_train)
View(pred_train)
library(caret)

confusionMatrix(pred_train,comp_train$ShelveLoc)
pred_test <- predict(comp_tree,comp_test)
confusionMatrix(pred_test,comp_test$ShelveLoc )
library(gmodels)
CrossTable(comp_train$ShelveLoc,pred_train)
