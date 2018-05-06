library(dplyr)
library(forecast)
library(tseries)
##Generate a AR(1) Model
AR1<-arima.sim(n=1000, list(ar=0.5), innov=rnorm(1000))
adf.test(AR1)
pacf(AR1)

MA1<-arima.sim(n=1000, list(ma=0.5), innov=rnorm(1000))
acf(MA1)

series<-arima.sim(n=1000, list(ar=c(0.5, -0.2)), innov=rnorm(1000))
pacf(series)

series1<-arima.sim(n=1000, list(ar=0.5,ma=0.5), innov=rnorm(1000))
acf(series1)## for MA
pacf(series1)## for AR



View(AirPassengers)
plot.ts(AirPassengers)
plot(decompose(AirPassengers))
#checkresiduals(AirPassengers)
adf.test(AirPassengers)
pacf(AirPassengers)
acf(AirPassengers, lag.max=50)


arima.fit<-auto.arima(AirPassengers)
arima.fit



setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\TS\\data")
dataset<-read.csv("nifty.csv", header = TRUE, stringsAsFactors = FALSE)
dim(dataset)
#Detect NAs
detectNA<- function(x){
  return(sum(is.na(x)))
}
sapply(dataset, detectNA)

detectNull<- function(x){
  return(sum(x=="null"))
}
sapply(dataset, detectNull)

dataset$Close[dataset$Close=='null']<-NA
dataset<-na.omit(dataset)
dataset$Close<-as.numeric(dataset$Close)

View(dataset)
class(dataset$Close)

dataSeries<-ts(dataset$Close, start = c(2013))
plot.ts(dataSeries)
adf.test(dataSeries)
View(dataSeries)
class(dataSeries)

arima.fit1<-auto.arima(dataSeries)
arima.fit1

y<-forecast(arima.fit1,h=5)
y
plot(y)



##Kings Data
kings <- read.csv("kings.csv", header = TRUE, stringsAsFactors = FALSE)
View(kings)
kingSeries<-ts(kings)
adf.test(kingSeries)

kingSeriesModel<-auto.arima(kingSeries)
kingSeriesModel


y<-predict(arimaKingSeries, n.ahead = 5)
y$pred

kingSeriesModelForecast<-forecast(kingSeriesModel, h=5)
kingSeriesModelForecast

plot.forecast(kingSeriesModel)