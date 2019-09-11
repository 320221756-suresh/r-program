View(CocaCola_Sales_Rawdata)
coca <- CocaCola_Sales_Rawdata
View(coca)
windows()
plot(coca$Sales,type="o")
df1 <- data.frame(Quarter = 1:4)

coca1 <- dummy(coca$Quarter)
x <- coca1
trakdata<-cbind(coca,x)
View(trakdata)
trakdata["t"]<- 1:42
View(trakdata)
trakdata["log_sales"]<-log(trakdata["Sales"])
trakdata["t_square"]<-trakdata["t"]*trakdata["t"]
attach(trakdata)
train<-trakdata[1:31,]

test<-trakdata[32:42,]
View(trakdata)
#linear
linear_model<-lm(Sales~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear 
#Exponential
expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo 
#Quadratic
Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad 
#Additive Seasonality
sea_add_model<-lm(Sales~QuarterQ1+QuarterQ2+QuarterQ3,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add
#Additive Seasonality with Linear
Add_sea_Linear_model<-lm(Sales~t+QuarterQ1+QuarterQ2+QuarterQ3,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-sqrt(mean((test$Sales-Add_sea_Linear_pred$fit)^2,na.rm=T))
rmse_Add_sea_Linear
#Additive Seasonality with Quadratic
Add_sea_Quad_model<-lm(Sales~t+t_square+QuarterQ1+QuarterQ2+QuarterQ3,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad
#Multiplicative Seasonality
multi_sea_model<-lm(log_sales~QuarterQ1+QuarterQ2+QuarterQ3,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea
#Multiplicative Seasonality Linear trend
multi_add_sea_model<-lm(log_sales~t+QuarterQ1+QuarterQ2+QuarterQ3,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_add_sea
#rmse table
table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)
# Multiplicative Seasonality Linear trend has least rmse