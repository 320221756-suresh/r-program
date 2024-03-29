st <- read.csv("C:/Users/Suresh/Downloads/50_Startups.csv")
View(st)
st <- st[,-4]
View(st)
summary(st)
pairs(st)
cor(st)
library(corpcor)

cor2pcor(cor(st))
attach(st)
model.sta <- lm(Profit~Marketing.Spend+Administration+R.D.Spend)
summary(model.sta)
model.pro <- lm(Profit~Marketing.Spend)
summary(model.pro)
model.ad <- lm(Profit~Marketing.Spend+Administration)
summary(model.ad)
model.sta <- lm(log(Profit)~Marketing.Spend+Administration+R.D.Spend)
summary(model.sta)
model.sta <- lm(log(Profit)~log(Marketing.Spend+Administration+R.D.Spend))
summary(model.sta)
model.sta <- lm((Profit)~log(Marketing.Spend+Administration+R.D.Spend))
summary(model.sta)
