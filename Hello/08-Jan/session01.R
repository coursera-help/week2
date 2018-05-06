library(TTR)
library(tseries)
library(forecast)
#install.packages("devtools")
library(forecast)
library(devtools)

data("AirPassengers")

apr=AirPassengers
apr
class(apr)
fix(apr)
plot(apr,col=c(3))

plot(decompose(apr))
plot(decompose(apr),col=(3))



apr.decom=decompose(apr,type = "multi")
plot(apr.decom)

trend=apr.decom$trend
trend
seasonal=apr.decom$seasonal
seasonal
random=apr.decom$random
random

ts.plot(cbind(trend,trend*seasonal),lty=1:2)

plot(stl(apr,"periodic"))