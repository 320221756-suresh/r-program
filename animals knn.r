Zoo <- read.csv("C:/Users/Suresh/Downloads/Zoo.csv")
View(Zoo)
str(Zoo)
table(Zoo$type)
Zoo$type <- factor(Zoo$type, levels = c(1:7), labels = c("Annelids","Arthropods","Bryozoa","Chordates","Cnidaria","Echinoderms","Molluscs"))
View(Zoo)
Zoo <- Zoo[,-1]
round(prop.table(table(Zoo$type)) * 100, digits = 1)
summary(Zoo)
Zoo_a <- Zoo[,-17]
zoo_train <- Zoo_a[1:80,]
zoo_test <- Zoo_a[81:101,]

zoo_labletrain <- Zoo[1:80,17]
zoo_labeltest <- Zoo[81:101,17]
library(class)
zoo_pred <- knn(train = zoo_train,test = zoo_test, cl = zoo_labletrain, k=15)
plot(zoo_pred)
library(gmodels)
CrossTable(x = zoo_labeltest, y = zoo_pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
##

zoo_scale <- as.data.frame(scale(Zoo_a))
zoo_train <- zoo_scale[1:80,]
zoo_test <- zoo_scale[81:101,]
View(zoo_scale)
library(class)
zoo_pred <- knn(train = zoo_train,test = zoo_test, cl = zoo_labletrain, k=15)
plot(zoo_pred)
library(gmodels)
CrossTable(x = zoo_labeltest, y = zoo_pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
#training 
zoo_train <- Zoo_a[1:80,]
zoo_test <- Zoo_a[81:101,]

zoo_pred <- knn(train = zoo_train,test = zoo_test, cl = zoo_labletrain, k=21)
CrossTable(x = zoo_labeltest, y = zoo_pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
zoo_pred <- knn(train = zoo_train,test = zoo_test, cl = zoo_labletrain, k=5)
CrossTable(x = zoo_labeltest, y = zoo_pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
zoo_pred <- knn(train = zoo_train,test = zoo_test, cl = zoo_labletrain, k=25)
CrossTable(x = zoo_labeltest, y = zoo_pred, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE)
