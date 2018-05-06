###Rainfall
setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\TS\\data")
data<-read.csv("mw2.csv", header = TRUE, stringsAsFactors = FALSE)
head(data)
detectNA<- function(x){
  return(sum(is.na(x)))
}
sapply(data, detectNA)

data<-na.omit(data)
data$YEAR<-as.Date(data$YEAR)
class(data$YEAR)
class(data$MWS)


dataSeries<-ts(data$MWS, start=c(1984,1), frequency = 1)
class(dataSeries)
plot.ts(dataSeries)

adf.test(dataSeries)
#adf.test(diff(unitSeries))

acf(diff(dataSeries))#q=3
pacf(diff(dataSeries))#p=3


##Model is(3,0,3)
arima.fit<-arima(dataSeries, order=c(1,0,1))
arima.fit

arima.fit<-arima(dataSeries, order=c(0,0,1))
arima.fit

arima.fit<-arima(dataSeries, order=c(1,0,0))
arima.fit



arima.fit<-arima(dataSeries, order=c(0,1,4))
arima.fit

auto.arima(dataSeries)

windforecast<-forecast(arima.fit, h=1)
plot(windforecast)

hist(windforecast$residuals)




###################
data1<-read.csv("humid.csv", header = TRUE, stringsAsFactors = FALSE)
head(data1)
data1<-na.omit(data1)
dataSeries1<-ts(data1$RH, start=c(1984,1), frequency = 1)
class(dataSeries1)
plot.ts(dataSeries1)

adf.test(dataSeries1)
#adf.test(diff(unitSeries))

acf(diff(dataSeries1))#q=3
pacf(diff(dataSeries1))#p=3


##Model is(2,1,2)
arima.fit<-arima(dataSeries1, order=c(1,0,1))
arima.fit

arima.fit<-arima(dataSeries1, order=c(0,0,1))
arima.fit

arima.fit<-arima(dataSeries1, order=c(1,0,0))
arima.fit

arima.fit<-arima(dataSeries1, order=c(2,0,0))
arima.fit

arima.fit<-arima(dataSeries1, order=c(0,0,2))
arima.fit

arima.fit<-arima(dataSeries1, order=c(2,0,2))
arima.fit

arima.fit<-arima(dataSeries1, order=c(2,1,2))
arima.fit

auto.arima(dataSeries1)

humidforecast<-forecast(arima.fit, h=1)
plot(humidforecast)

hist(humidforecast$residuals)