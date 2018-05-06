library(tseries)
library(fpp)
library(devtools)
library(caTools)
library(forecast)
View(oil)
plot.ts(oil)

oilSeries<-ts(oil, start=c(1963))
plot.ts(oilSeries)
decompose(oilSeries)

oilSeriesForecast<-HoltWinters(oilSeries, beta = FALSE, gamma = FALSE)
oilSeriesForecast
oilSeriesForecast$fitted

plot(oilSeriesForecast)
oilSeriesForecast2<-forecast(oilSeriesForecast, h=8)
plot(oilSeriesForecast2)
checkresiduals(oilSeriesForecast2)
Box.test(oilSeriesForecast2$residuals, lag=20, type="Ljung-Box")##p>0.05, doesnot exhibit auto correlation
plot.ts(oilSeriesForecast2$residuals)