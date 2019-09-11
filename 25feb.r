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


#datapca <- dataset[,c(6,7,8)]
#View(datapca)
#cor(datapca)
#pcaObj <- princomp(datapca,cor = TRUE, scores = TRUE, covmat = NULL)
#summary(pcaObj)
#str(pcaObj)
#loadings(pcaObj)
#plot(pcaObj)
#plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
#pcascore <- pcaObj$scores

#after doing PCA model decided to remove lone duration coloumn in data set
#dataset <- dataset[,-8]
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
dataset$Loan.Duration <- as.factor(dataset$Loan.Duration)


dataset$StateName <- as.numeric(dataset$StateName)
dataset$LocationName <- as.numeric(dataset$LocationName)
dataset$BrandTypeName <- as.numeric(dataset$BrandTypeName)
dataset$FinanceAmt <- as.numeric(dataset$FinanceAmt)
dataset$Instalment <- as.numeric(dataset$Instalment)
dataset$HubName <- as.numeric(dataset$HubName)
dataset$LoanTypeName <- as.numeric(dataset$LoanTypeName)
dataset$Loan.Duration <- as.numeric(dataset$Loan.Duration)
str(dataset)
dataset$Defaulter <- factor(dataset$Defaulter, levels = c(0,1), labels = c("defaulter","not defaulter"))
str(dataset)

#train <- dataset[1:290160,]
#test <- dataset[290160:414515,]
library(caret)
library(C50)
fld <- createFolds(dataset$Defaulter,k= 10, list = TRUE, returnTrain = FALSE)
names(fld)[1] <- "train"
dataw <- dataset[sample(nrow(dataset)),]
fld <- cut(seq(1,nrow(dataset)),breaks=10,labels=FALSE)


for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(fld==i,arr.ind=TRUE)
  testData <- dataset[testIndexes, ]
  trainData <- dataset[-testIndexes, ]
  #Use the test and train data partitions however you desire...
}

trainpca <- trainData[,c(6,7,8)]
View(trainpca)
cor(trainpca)
pcaObj <- princomp(trainpca,cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj)
pcascore1 <- pcaObj$scores
str(pcascore1)
trainData <- cbind(trainData[,c(1,2,3,4,5,9)],pcascore1[,1:2])

##test
trainpca2 <- testData[,c(6,7,8)]
View(trainpca2)
cor(trainpca2)
pcaObj2 <- princomp(trainpca2,cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj2)
str(pcaObj2)
loadings(pcaObj2)
plot(pcaObj2)
pcascore2 <- pcaObj2$scores
str(pcascore2)
testData <- cbind(testData[,c(1,2,3,4,5,9)],pcascore2[,1:2])

cd <- C5.0(trainData[,-6],trainData$Defaulter)
pred <- predict.C5.0(cd,testData[,-6])
mean(pred==testData$Defaulter)


#######
library(e1071)
attach(dataset)
model <- naiveBayes(Defaulter~., data = trainData)
summary(model)
model

pred1 <- predict(model,testData)
library(gmodels)
CrossTable(pred1,testData$Defaulter)
mean(pred1==testData$Defaulter)

train_label1 <- trainData[,8]
test_label1 <-testData[,8]

############
library(class)
pred <- knn(train = trainData[,-8], test = testData[,-8], cl = train_label1, k=5)
CrossTable(x=test_label1, y=pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
mean(pred==testData$Defaulter)

######
score = list()

LOOCV_function = function(x,label){
  for(i in 1:nrow(x)){
    training = x[-i,]
    model <- C5.0(trainData[,-8],trainData$Defaulter)
    validation = x[i,]
    pred4 = predict(model, validation[,setdiff(names(validation),label)])
    #score[[i]] = rmse(pred, validation[[label]]) # score/error of ith fold
    score[[i]]=sqrt(mean((validation[label]-pred)^2,na.rm = T))
  }
  return(unlist(score)) # returns a vector
}

LOOCV_function(dataset[,-8],dataset$Defaulter)

?RMSE

################
library(randomForest)
attach(dataset)
memory.size(max=TRUE)
model2 <- randomForest(Defaulter~., data = trainData, importance= TRUE)
plot(model2)
########
acc <- c()
for(i in 15){
library(caret)
intrainingdata <- createDataPartition(dataset$Defaulter, p=0.6, list = F)
training <- dataset[intrainingdata,]
testing <- dataset[intrainingdata,]
dim(testing)
library(C50)
cd <- C5.0(training[,-8],training$Defaulter, trails=10)
pred <- predict.C5.0(cd,testing[,-8])
cdtable <- table(testing$Defaulter,pred)
accurcy <- sum(diag(cdtable))/sum(cdtable)

#window()
#plot(cd)
acc <- c(acc, accurcy)
}
acc
summary(acc)
