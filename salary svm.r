salaryData_Train <- read.csv("C:/Users/Suresh/Downloads/SalaryData_Train.csv")


salaryData_Train$workclass <- as.numeric(salaryData_Train$workclass)
salaryData_Train$education <- as.numeric(salaryData_Train$education)
salaryData_Train$maritalstatus <- as.numeric(salaryData_Train$maritalstatus)
salaryData_Train$occupation <- as.numeric(salaryData_Train$occupation)
salaryData_Train$relationship <- as.numeric(salaryData_Train$relationship)
salaryData_Train$race <- as.numeric(salaryData_Train$race)
salaryData_Train$sex <- as.numeric(salaryData_Train$sex)
salaryData_Train$native <- as.numeric(salaryData_Train$native)
View(salaryData_Train)
salaryData_Test <- read.csv("C:/Users/Suresh/Downloads/SalaryData_Test.csv")

salaryData_Test$workclass <- as.numeric(salaryData_Test$workclass)
salaryData_Test$education <- as.numeric(salaryData_Test$education)
salaryData_Test$maritalstatus <- as.numeric(salaryData_Test$maritalstatus)
salaryData_Test$occupation <- as.numeric(salaryData_Test$occupation)
salaryData_Test$relationship <- as.numeric(salaryData_Test$relationship)
salaryData_Test$race <- as.numeric(salaryData_Test$race)
salaryData_Test$sex <- as.numeric(salaryData_Test$sex)
salaryData_Test$native <- as.numeric(salaryData_Test$native)


salary_train <- salaryData_Train
salary_test <- salaryData_Test

table(salary_train$Salary)
table(salary_test$Salary)
str(salary1)
library(kernlab)
library(caret)
model <- ksvm(Salary~., data = salary_train,kernel = "vanilladot")
model
summary(model)
pred <- predict(model, salary_test)
mean(salary_test$Salary==pred)

model1 <- ksvm(Salary~., data = salary_train,kernel ="rbfdot")
model1
summary(model1)
pred <- predict(model1, salary_test)
mean(salary_test$Salary==pred)
