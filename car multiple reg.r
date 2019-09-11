ty <- `ToyotaCorolla.(1)`
ty <- ty[,-1]
View(ty)
ty <- ty[,c(2,3,6,8,12,13,15,16,17)]
View(ty)
summary(ty)
pairs(ty)
cor(ty)
attach(TY)
attach(ty)
model.ty <- lm(Price~.,data = ty)
model.ty
boxplot(ty)
library(corpcor)
cor2pcor(cor(ty))
model.ty <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = ty)
model.ty
summary(model.ty)
model.tyage <- lm(Price~Age_08_04,data = ty)
summary(model.tyage)
vif(model.ty)
panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(ty,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")
influence.measures(model.ty)
library(car)
windows()
influenceIndexPlot(model.ty,id.n=3)
influencePlot(model.ty,id.n=3)
model.ty <- lm(log(Price)~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = ty)
model.ty
summary(model.ty)
model.ty <- lm(log(Price)~log(Age_08_04)+log(KM)+log(HP)+log(cc)+log(Doors)+log(Gears)+log(Quarterly_Tax)+log(Weight))
model.ty
summary(model.ty)
