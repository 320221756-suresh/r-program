install.packages('AER')
data(Affairs,package="AER")
str(Affairs)
table(Affairs$affairs)
affai <- Affairs
affai$gender <- as.numeric(affai$gender)
affai$children <- as.numeric(affai$children)
View(affai)

str(affai)
model <- lm(affairs~., data = affai)
model
pred <- predict(model,affai)
pred
plot(affai$affairs, pred)
model2 <- glm(affairs~.,data=affai)


prob <- predict(model2,affai,type="response")
summary(model2)
View(prob)
status <- NULL
status <- ifelse(prob>=3,"fraud","not fraud")
affai[,"status"] <- status
View(affai)
