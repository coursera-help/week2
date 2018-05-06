library(dplyr)
library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)
library(ggplot2)

setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\TS\\data")
m2Data<-read.csv("temperature.csv", header = TRUE, stringsAsFactors = FALSE)
head(m2Data)
#Detect NAs
detectNA<- function(x){
  return(sum(is.na(x)))
}
sapply(m2Data, detectNA)

detectNull<- function(x){
  return(sum(x=="null"))
}
sapply(m2Data, detectNull)

detectSpace<- function(x){
  return(sum(trimws(x)==""))
}
sapply(m2Data, detectSpace)




m2Data<-na.omit(m2Data)
m2Data$YEAR<-as.Date(m2Data$YEAR)
class(m2Data$YEAR)
class(m2Data$MMAX)


##MEAN MAXIMUM TEMPERATURE (in Deg. C)###############################
m2Series<-ts(m2Data$MMAX, start=c(1984,1), frequency = 1)
class(m2Series)
plot.ts(m2Series)

adf.test(m2Series)
#adf.test(diff(unitSeries))

acf(diff(m2Series))#q=2
pacf(diff(m2Series))#p=2


##Model is(2,0,2)
arima.fit<-arima(m2Series, order=c(1,0,1))
arima.fit

arima.fit<-arima(m2Series, order=c(0,0,1))
arima.fit

arima.fit<-arima(m2Series, order=c(1,0,0))
arima.fit

arima.fit<-arima(m2Series, order=c(2,0,0))
arima.fit

arima.fit<-arima(m2Series, order=c(0,0,2))
arima.fit

arima.fit<-arima(m2Series, order=c(2,0,2))
arima.fit

auto.arima(m2Series)

m2forecast<-forecast(arima.fit, h=1)
plot(m2forecast)



hist(m2forecast$residuals)






###Rainfall
setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\TS\\data")
rainfallData<-read.csv("rainfallData.csv", header = TRUE, stringsAsFactors = FALSE)
head(rainfallData)
detectNA<- function(x){
  return(sum(is.na(x)))
}
sapply(rainfallData, detectNA)

rainfallData<-na.omit(rainfallData)
rainfallData$YEAR<-as.Date(rainfallData$YEAR)
class(rainfallData$YEAR)
class(rainfallData$TMRF)


rainfallSeries<-ts(rainfallData$TMRF, start=c(1984,1), frequency = 1)
class(rainfallSeries)
plot.ts(rainfallSeries)

adf.test(rainfallSeries)
#adf.test(diff(unitSeries))

acf(diff(rainfallSeries))#q=2
pacf(diff(rainfallSeries))#p=2


##Model is(2,0,2)
arima.fit<-arima(rainfallSeries, order=c(1,0,1))
arima.fit

arima.fit<-arima(rainfallSeries, order=c(0,0,1))
arima.fit

arima.fit<-arima(rainfallSeries, order=c(1,0,0))
arima.fit

arima.fit<-arima(rainfallSeries, order=c(2,0,0))
arima.fit

arima.fit<-arima(rainfallSeries, order=c(0,0,2))
arima.fit

arima.fit<-arima(rainfallSeries, order=c(2,0,2))##Best Model
arima.fit

auto.arima(rainfallSeries)

rainforecast<-forecast(arima.fit, h=1)
plot(rainforecast)

hist(rainforecast$residuals)



