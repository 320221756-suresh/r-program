crime <- crime_data
crime <- crime[,-1]
View(crime)
normalized_data<-scale(crime)

#no of clusters
wss<-(nrow(crime)-1)*sum(apply(crime,2,var))
for(i in 2:15) wss[i]<-sum(kmeans(crime,centers = i)$withinss)
plot(1:15,wss,type = "b", xlab = "No of clusters",ylab = "Avg distance")

#3 clusters 

d <- dist(normalized_data, method = "euclidean") 
fit <- hclust(d, method="complete")
plot(fit) 
plot(fit, hang=-1)
rect.hclust(fit, k=3, border="red")
groups <- cutree(fit, k=3)
membership<-as.matrix(groups) 
final <- data.frame(crime, membership)

View(final)
