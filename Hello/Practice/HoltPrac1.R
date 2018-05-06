library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)


plot.ts(eggs)
#beer<-aggregate(ausbeer)
eggstrain<-window(eggs, end=1989)
eggstest<-window(eggs, start=1990)



#Holt Linear Model(Additive)
cat("\014")
f5 <- holt(eggstrain, initial="simple", h=100)
summary(f5)
f5$model
accuracy(f5, eggstest)
plot(residuals(f5))###This model is stationary or white noise
acf(residuals(f5))
checkresiduals(f5)
###This is the best fit model

#Holt Exponential Trend Model(Multiplicative)
cat("\014")
f6 <- holt(eggstrain, initial="simple", exponential=TRUE, h=100)
f6
f6$model
accuracy(f6, eggstest)

#Holt Linear Damped(Additive)
cat("\014")
f7<-holt(eggstrain, damped=TRUE, initial="optimal", h=100)
f7
f7$model
accuracy(f7, eggstest)

#Holt Exponential damped trend(Multiplicative)
cat("\014")
f8<-holt(eggstrain, damped=TRUE, initial="optimal", exponential=TRUE, h=100)
f8
accuracy(f8, eggstest)
