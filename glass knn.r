glass <- read.csv("C:/Users/Suresh/Downloads/glass.csv")
View(glass)
table(glass$Type)
str(glass)
glass$Type <- factor(glass$Type, levels = c(1,2,3,5,6,7), labels = c("Float","Safety Laminated","Obscured","Annealed","Tinted","Tempered"))
View(glass)
round(prop.table(table(glass$Type)) * 100, digits = 1)
summary(glass)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
glass_n <- as.data.frame(lapply(glass[1:9], normalize))

glass_train <- glass_n[1:160,]
glass_test <- glass_n[161:214,]

glass_labletrain <- glass[1:160,10]
glass_labletest <- glass[161:214,10]
str(glass_test)
str(glass_labletest)
library(class)
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=15)
library(gmodels)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
glass_scale <- scale(glass[,-10])

glass_train <- glass_scale[1:160,]
glass_test <- glass_scale[161:214,]
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=15)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=1)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=11)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

glass_train <- glass_n[1:160,]
glass_test <- glass_n[161:214,]
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=15)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)

glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=1)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_labletrain, k=11)
CrossTable(x=glass_labletest, y=glass_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
