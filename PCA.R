wine <- read.csv("C:/Users/Suresh/Downloads/wine.csv")
View(wine)
data <- wine[,-1]
attach(data)
cor(data)
pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj)
biplot(pcaObj)
plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$scores
pcathree <- pcaObj$scores[,1:3]
data<-cbind(data,pcathree)
View(data)
clus_data<-data[,14:16]
norm_clus<-scale(clus_data) 
View(norm_clus)


#kmeans
wss<-(nrow(clus_data)-1)*sum(apply(clus_data,2,var))
for(i in 2:15) wss[i]<-sum(kmeans(clus_data,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "Avg distance")

dist1<-dist(norm_clus,method = "euclidean")
fit1<-hclust(dist1,method="complete")
plot(fit1,hang = -1)

rect.hclust(fit1, k=6, border="red")
groups<-cutree(fit1,6) 

membership_1<-as.matrix(groups) 

View(membership_1)

final1<-cbind(membership_1,data) 
View(final1)
View(aggregate(final1[,-c(2,15:17)],by=list(membership_1),FUN=mean))
