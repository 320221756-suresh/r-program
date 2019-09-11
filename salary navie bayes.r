salary_train <- read.csv("C:/Users/Suresh/Downloads/SalaryData_Train.csv")
View(salary_train)
salary_test <- read.csv("C:/Users/Suresh/Downloads/SalaryData_Test.csv")
View(salary_test)
table(salary_train$Salary)
table(salary_test$Salary)
str(salary_train)

prop.table(table(salary_train$Salary))
nrow(salary_train)
library(e1071)
attach(salary_train)
model <- naiveBayes(Salary~., data = salary_train)
summary(model)
model
salary_pred <- predict(model,salary_test)
library(gmodels)
CrossTable(salary_pred,salary_test$Salary)
mean(salary_pred==salary_test$Salary)
###

model1 <- naiveBayes(Salary~.,data = salary_train, laplace = 1)
summary(model1)
model1
salary_pred <- predict(model1,salary_test)
library(gmodels)
CrossTable(salary_pred,salary_test$Salary)
mean(salary_pred==salary_test$Salary)
