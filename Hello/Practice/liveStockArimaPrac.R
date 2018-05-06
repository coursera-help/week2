library(forecast)
library(tseries)
library(fpp)
library(devtools)
library(caTools)


View(livestock)
plot.ts(livestock)
adf.test(livestock)

livestockSeries<-livestock
#decompose(livestockSeries)


livestockSeries1<-diff(livestockSeries, differences = 1)
adf.test(livestockSeries)

livestockSeries2<-diff(livestockSeries, differences = 2)
adf.test(livestockSeries2)
plot.ts(livestockSeries2)

Acf(diff(livestockSeries2))
acf(livestockSeries2)##q=1
pacf(livestockSeries2)##p=1

 ##Model is (1,2,1)
arima.fit<-arima(livestockSeries2, order=c(1,0,1))
arima.fit

arima.fit<-arima(livestockSeries2, order=c(0,0,1))
arima.fit

arima.fit<-arima(livestockSeries2, order=c(1,0,0))
arima.fit

arima.fit<-arima(livestockSeries, order=c(1,2,1))
arima.fit

##Validate
livestockforecast<-forecast(arima.fit, h=8)
plot(livestockforecast)

Box.test(livestockforecast$residuals, lag=20, type="Ljung-Box")
hist(livestockforecast$residuals)
Acf(livestockforecast$residuals)
checkresiduals(livestockforecast$residuals)
plot(livestockforecast$residuals)


