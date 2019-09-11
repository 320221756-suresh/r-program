library(readxl) 
storing<- read_xlsx(file.choose(), 2)
airlines <- storing
airlines <- airlines[,c(-1,-12)]
normalized_data <- scale(airlines)
View(normalized_data)
km <- kmeans(normalized_data,4)
attributes(km)

library(animation)

km <- kmeans.ani(normalized_data, 4)
km$centers


km_8 <- kmeans(normalized_data,8)
km_8 <- kmeans.ani(normalized_data,8)
wss<-(nrow(normalized_data)-1)*sum(apply(normalized_data,2,var))
for(i in 2:15) wss[i]<-sum(kmeans(normalized_data,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "Avg distance")
d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="complete")
plot(fit)
plot(fit, hang=-1)
rect.hclust(fit, k=8, border="red")
groups <- cutree(fit, k=8)
membership<-as.matrix(groups) 
final <- data.frame(airlines, membership)
View(final)
