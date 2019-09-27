library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
library(Quandl)
library(DMwR)
library(timeDate)


sales_data<-read.csv('/home/1320B29/phd/Train.csv')



colnames(sales_data)[4] <- "sales"
summary(sales_data)



str(sales_data)



men_sales<-filter(sales_data, ProductCategory %in% c("MenClothing"))
head(men_sales)
summary(men_sales)
men_sales$sales <- na.kalman(men_sales$sales,model='auto.arima')
men_train<-men_sales[1:(nrow(men_sales)-5),]
men_test<-men_sales[(nrow(men_sales)-4):(nrow(men_sales)),]




women_sales<-filter(sales_data, ProductCategory %in% c("WomenClothing"))
head(women_sales)
summary(women_sales)
women_sales$sales <- na.kalman(women_sales$sales,model='auto.arima')
women_train<-women_sales[1:(nrow(women_sales)-5),]
women_test<-women_sales[(nrow(women_sales)-4):(nrow(women_sales)),]



other_sales <- filter(sales_data, ProductCategory %in% c("OtherClothing"))
head(other_sales)
summary(other_sales)
other_sales$sales <- na.kalman(other_sales$sales,model='auto.arima')
other_train<-other_sales[1:(nrow(other_sales)-5),]
other_test<-other_sales[(nrow(other_sales)-4):(nrow(other_sales)),]



men_series<-ts(men_train$sales,frequency=12)
women_series <- ts(women_train$sales,frequency=12)
other_series <- ts(other_train$sales,frequency=12)



plot(men_series,type="l",lwd=3,col="red",xlab="month",ylab="men_sales",main="Time series plot for men_sales")

plot(women_series,type="l",lwd=3,col="red",xlab="month",ylab="women_sales",main="Time series plot for women_sales")

plot(other_series,type="l",lwd=3,col="red",xlab="month",ylab="other_sales",main="Time series plot for other_sales")



men_series_decompose<-decompose(men_series)
plot(men_series_decompose,col='Red')

women_series_decompose<-decompose(women_series)
plot(women_series_decompose,col='Red')

other_series_decompose<-decompose(other_series)
plot(other_series_decompose,col='Red')



par(mfrow=c(3,2))
acf(men_series,lag=30)
pacf(men_series,lag=30)

acf(women_series,lag=30)
pacf(women_series,lag=30)

acf(other_series,lag=30)
pacf(other_series,lag=30)



ndiffs(men_series)
ndiffs(women_series)
ndiffs(other_series)
par(mfrow=c(3,2))
acf(diff(men_series,lag=1),lag=30)
pacf(diff(men_series,lag=1),lag=30)

acf(diff(women_series,lag=1),lag=30)
pacf(diff(women_series,lag=1),lag=30)

acf(diff(other_series,lag=1),lag=30)
pacf(diff(other_series,lag=1),lag=30)

st_men_series = diff(men_series,lag=1)
st_women_series = diff(women_series,lag=1)
st_men_series = diff(other_series,lag=1)



#par(mfrow=c(3,1))

fit_men_series_sma<-SMA(men_series,n=2)
pred_men_series_sma<-forecast(fit_men_series_sma[!is.na(fit_men_series_sma)],h=5)
plot(pred_men_series_sma)

fit_women_series_sma<-SMA(women_series,n=2)
pred_women_series_sma<-forecast(fit_women_series_sma[!is.na(fit_women_series_sma)],h=5)
plot(pred_women_series_sma)

fit_other_series_sma<-SMA(other_series,n=2)
pred_other_series_sma<-forecast(fit_other_series_sma[!is.na(fit_other_series_sma)],h=5)
plot(pred_other_series_sma)



sma_Train_men_series_Mape <- regr.eval(men_series[2:length(men_series)],fit_men_series_sma[2:length(men_series)])
sma_Test_men_series_Mape <- regr.eval(men_test$sales,pred_men_series_sma$mean)
sma_Train_men_series_Mape
sma_Test_men_series_Mape

sma_Train_women_series_Mape <- regr.eval(women_series[2:length(women_series)],fit_women_series_sma[2:length(women_series)])
sma_Test_women_series_Mape <- regr.eval(women_test$sales,pred_women_series_sma$mean)
sma_Train_women_series_Mape
sma_Test_women_series_Mape

sma_Train_other_series_Mape <- regr.eval(other_series[2:length(other_series)],fit_other_series_sma[2:length(other_series)])
sma_Test_other_series_Mape <- regr.eval(other_test$sales,pred_other_series_sma$mean)
sma_Train_other_series_Mape
sma_Test_other_series_Mape


#Function to write results to predict.csv
write_results <- function(target1,target2,target3){
  submission_file <- read.csv('template.csv')
  submission_file$target[1:12] <- target1
  submission_file$target[13:24] <- target2
  submission_file$target[25:36] <- target3
  write.csv(submission_file,'predictions.csv',row.names=F)
}



men_total<-ts(men_sales$sales,frequency=12)
women_total <- ts(women_sales$sales,frequency=12)
other_total <- ts(other_sales$sales,frequency=12)



fit_men_series_sma<-SMA(men_total,n=2)
pred_men_series_sma<-forecast(fit_men_series_sma[!is.na(fit_men_series_sma)],h=12)

plot(pred_men_series_sma)

fit_women_series_sma<-SMA(women_total,n=2)
pred_women_series_sma<-forecast(fit_women_series_sma[!is.na(fit_women_series_sma)],h=12)
plot(pred_women_series_sma)
pred_women_series_sma$mean

fit_other_series_sma<-SMA(other_total,n=2)
pred_other_series_sma<-forecast(fit_other_series_sma[!is.na(fit_other_series_sma)],h=12)
plot(pred_other_series_sma)
write_results(pred_women_series_sma$mean,pred_men_series_sma$mean,pred_other_series_sma$mean)




fit_men_series_sma<-SMA(men_total,n=3)
fit_women_series_sma<-SMA(women_total,n=3)
fit_other_series_sma<-SMA(other_total,n=3)

pred_men_series_sma<-forecast(fit_men_series_sma[!is.na(fit_men_series_sma)],h=12)
pred_women_series_sma<-forecast(fit_women_series_sma[!is.na(fit_women_series_sma)],h=12)
pred_other_series_sma<-forecast(fit_other_series_sma[!is.na(fit_other_series_sma)],h=12)

write_results(pred_women_series_sma$mean,pred_men_series_sma$mean,pred_other_series_sma$mean)



men_holtwinters<-HoltWinters(men_total,beta=TRUE,gamma=TRUE,seasonal='multiplicative')
women_holtwinters<-HoltWinters(women_total,beta=TRUE,gamma=TRUE,seasonal='multiplicative')
other_holtwinters<-HoltWinters(other_total,beta=TRUE,gamma=TRUE,seasonal='multiplicative')



holtforecast_men<-forecast(men_holtwinters,h=12)
plot(holtforecast_men)

holtforecast_women<-forecast(women_holtwinters,h=12)
plot(holtforecast_women)

holtforecast_other<-forecast(other_holtwinters,h=12)
plot(holtforecast_other)




write_results(holtforecast_women$mean,holtforecast_men$mean,holtforecast_other$mean)



arima_men_model1 <- arima(men_total,c(0,1,0))
arima_women_model1 <- arima(women_total,c(0,1,0))
arima_other_model1 <- arima(other_total,c(0,1,0))

arima1_men <- forecast(arima_men_model1,h=12)
arima1_women <- forecast(arima_women_model1,h=12)
arima1_other <- forecast(arima_other_model1,h=12)

write_results(arima1_women$mean,arima1_men$mean,arima1_other$mean)



arima_men_model2 <- arima(men_total,c(1,1,1))
arima_women_model2 <- arima(women_total,c(1,1,1))
arima_other_model2 <- arima(other_total,c(1,1,1))

arima2_men <- forecast(arima_men_model1,h=12)
arima2_women <- forecast(arima_women_model1,h=12)
arima2_other <- forecast(arima_other_model1,h=12)

write_results(arima2_women$mean,arima2_men$mean,arima2_other$mean)



arima_men_model3 <- arima(men_total,c(0,1,1))
arima_women_model3 <- arima(women_total,c(0,1,1))
arima_other_model3 <- arima(other_total,c(0,1,1))

arima3_men <- forecast(arima_men_model1,h=12)
arima3_women <- forecast(arima_women_model1,h=12)
arima3_other <- forecast(arima_other_model1,h=12)

write_results(arima3_women$mean,arima3_men$mean,arima3_other$mean)



auto_arima_men<-auto.arima(men_total,ic='aic')
auto_arima_women<-auto.arima(women_total,ic='aic')
auto_arima_other<-auto.arima(other_total,ic='aic')

auto_arima1_men <- forecast(auto_arima_men,h=12)
auto_arima1_women <- forecast(auto_arima_women,h=12)
auto_arima1_other <- forecast(auto_arima_other,h=12)

write_results(auto_arima1_women$mean,auto_arima1_men$mean,auto_arima1_other$mean)



