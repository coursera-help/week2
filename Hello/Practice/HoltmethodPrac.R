library(tseries)
library(fpp)
library(devtools)
library(caTools)
plot.ts(livestock)

livestockSeries<-ts(livestock, start=c(1960))
plot.ts(livestockSeries)

##Aplying HoltWinters Model when Beta and gamma is FALSE
livestockSeriesForecast<-HoltWinters(livestockSeries, beta = FALSE, gamma = FALSE)
livestockSeriesForecast
livestockSeriesForecast$fitted
plot(livestockSeriesForecast)


livestockSeriesForecast2<-forecast(livestockSeriesForecast, h=8)
plot(livestockSeriesForecast2)

checkresiduals(livestockSeriesForecast2)
Box.test(livestockSeriesForecast2$residuals, lag=20, type="Ljung-Box")##p>0.05, doesnot exhibit auto correlation
Acf(livestockSeriesForecast2$residuals, lag.max=20)
plot.ts(livestockSeriesForecast2$residuals)
hist(livestockSeriesForecast2$residuals, col="red")



##Aplying HoltWinters Model when gamma is FALSE
livestockSeriesForecast3<-HoltWinters(livestockSeries, gamma = FALSE)
livestockSeriesForecast3
livestockSeriesForecast3$fitted
plot(livestockSeriesForecast3)


livestockSeriesForecast4<-forecast(livestockSeriesForecast3, h=8)
plot(livestockSeriesForecast4)

checkresiduals(livestockSeriesForecast4)
Box.test(livestockSeriesForecast4$residuals, lag=20, type="Ljung-Box")##p>0.05, doesnot exhibit auto correlation
Acf(livestockSeriesForecast4$residuals, lag.max=20)
plot.ts(livestockSeriesForecast4$residuals)
hist(livestockSeriesForecast4$residuals)


