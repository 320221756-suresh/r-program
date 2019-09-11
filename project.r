PerformanceProcessed <- read.csv("C:/Users/Suresh/Downloads/PerformanceProcessed.csv")
View(PerformanceProcessed)
dataset <- PerformanceProcessed
#checking na values in data set
sum(is.na(dataset))
# removeing data entry null values
dataset <- dataset[-482807,]
View(dataset)
str(dataset)
library(gsubfn)
#data cleaning
dataset$StateName<-gsub("ANDHRA PRADESH ONE","Telengana",dataset$StateName)
dataset$LocationName <- gsub("CHIRAWA","CHIRALA",dataset$LocationName)
dataset$LocationName <- gsub("ADALAJ TW","ADAJAN TW",dataset$LocationName)
dataset$BrandTypeName <- gsub("ESCORT CONSTRUCTION EQUIPMENTLTD","ESCORTS CONSTRUCTION EQUIPMENT LTD",dataset$BrandTypeName)
dataset$BrandTypeName <- gsub("ESCORTS LTD","ESCORT",dataset$BrandTypeName)
dataset$BrandTypeName <- gsub("JAKSON LIMITED","JAKSON",dataset$BrandTypeName)
dataset$BrandTypeName <- gsub("SANY HEAVY INDUSTRY INDIA PVT.LTD.","SANY HEAVY INDUSTRY INDIA PVT LTD",dataset$BrandTypeName)
dataset$BrandTypeName <- gsub("TAFE (TRACTOR AND FARM EQUIPMENT LTD)","TAFE",dataset$BrandTypeName)
dataset$BrandTypeName <- gsub("TOYOTA KIRLOSKAR MOTORS","TOYOTA MOTORS",dataset$BrandTypeName)

write.csv(dataset,"G://classes//assignments//dataset.csv")

#PCA model


datapca <- dataset[,c(6,7,8)]
View(datapca)
cor(datapca)
pcaObj <- princomp(datapca,cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj)
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")

#after doing PCA model decided to remove lone duration coloumn in data set
dataset <- dataset[,-8]
str(dataset)
boxplot(dataset$FinanceAmt)
#as this dataset belongs to banking lone set we should not remove outliers 

#removing data as per company condition 
dataset <- subset(dataset,LoanTypeName!="BUSES")
dataset <- subset(dataset,LoanTypeName!="CAR")
dataset <- subset(dataset,LoanTypeName!="CONSTRUCTION EQUIPMENTS")
dataset <- subset(dataset,LoanTypeName!="HEAVY COMMERCIAL VEHICLE")
dataset <- subset(dataset,LoanTypeName!="INTERMEDIATE COMMERCIAL VEHICLE")
dataset <- subset(dataset,LoanTypeName!="LIGHT COMMERCIAL VEHICLES")
dataset <- subset(dataset,LoanTypeName!="MEDIUM COMMERCIAL VEHICLE")
dataset <- subset(dataset,LoanTypeName!="MULTI UTILITY VEHICLE")
dataset <- subset(dataset,LoanTypeName!="SMALL COMMERCIAL VEHICLE")
dataset <- subset(dataset,LoanTypeName!="USED VEHICLES")
str(dataset)
dataset$StateName <- as.factor(dataset$StateName)
dataset$LocationName <- as.factor(dataset$LocationName)
dataset$BrandTypeName <- as.factor(dataset$BrandTypeName)
dataset$FinanceAmt <- as.factor(dataset$FinanceAmt)
dataset$Instalment <- as.factor(dataset$Instalment)


dataset$StateName <- as.numeric(dataset$StateName)
dataset$LocationName <- as.numeric(dataset$LocationName)
dataset$BrandTypeName <- as.numeric(dataset$BrandTypeName)
dataset$FinanceAmt <- as.numeric(dataset$FinanceAmt)
dataset$Instalment <- as.numeric(dataset$Instalment)
dataset$HubName <- as.numeric(dataset$HubName)
dataset$LoanTypeName <- as.numeric(dataset$LoanTypeName)
str(dataset)
dataset$Defaulter <- factor(dataset$Defaulter, levels = c(0,1), labels = c("fraud","not fraud"))
str(dataset)
#partition the data
train <- dataset[1:290160,]
test <- dataset[290160:414515,]

data <- dataset[,-8]
data_train <- data[1:290160,] 
data_test <- data[290160:414515,]
train_label <- dataset[1:290160,8]
test_label <- dataset[290160:414515,8]
library(class)
pred <- knn(train = data_train,test = data_test, cl = train_label, k=15)
plot(pred)
library(gmodels)
CrossTable(x = test_label, y = pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
mean(pred==dataset$Defaulter)


pred <- knn(train = data_train, test = data_test, cl = train_label, k=1)
CrossTable(x=test_label, y=pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
mean(pred==dataset$Defaulter)
pred <- knn(train = data_train, test = data_test, cl = train_label, k=11)
CrossTable(x=test_label, y=pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

mean(pred==test_label)

###############
library(e1071)
attach(dataset)
model <- naiveBayes(Defaulter~., data = train)
summary(model)
model

pred1 <- predict(model,test)
library(gmodels)
CrossTable(pred1,test$Defaulter)
mean(pred1==test$Defaulter)
