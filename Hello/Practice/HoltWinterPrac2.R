library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)


plot.ts(visitors)## Multiplicative Model
decompose(visitors)## Seasonality Component present

##Split the dataset
visitorstrain<-window(visitors, end=1997.99)
visitorstest<-window(visitors, start=1998)

cat("\014")
f1 <- hw(visitorstrain, seasonal="multiplicative", exponential=TRUE, h=88)
f1
accuracy(f1, visitorstest)



cat("\014")
f2<-holt(visitorstrain, seasonal="multiplicative", damped=TRUE, h=88)
summary(f2)
accuracy(f2, visitorstest)
residuals(f2)
plot(residuals(f2))
checkresiduals(f2)###Not white Nose
####Holt Winter Seasonal Multiplicative Damped Model is better

cat("\014")
f3<-holt(visitorstrain, seasonal="multiplicative", damped=TRUE, intial="optimal", exponential=TRUE, h=88)
f3
accuracy(f3, visitorstest)



