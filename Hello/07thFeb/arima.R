library(tseries)

##Augmented Dicky Fuller
adf.test(nifty)
adf.test(nifty1)

##AR(1)
AR1<-arima.sim(n=100, list(ar=0.9), innov=rnorm(100))
adf.test(AR1)
plot(AR1)
mean(AR1)

arima(AR1,order=c(1,0,0))

##AR(2)
AR2<-arima.sim(n=100, list(ar=c(0.2, 0.3)), innov=rnorm(100))
adf.test(AR2)
var(AR1)

AR2<-arima.sim(n=100, list(ar=c(0.3, 0.3)), innov=rnorm(100))
adf.test(AR2)
diffAR2<-diff(AR2, 1)
arima(diffAR2, order = c(2,0,0))

##Auto Coversion to Stationary and check coeff
arima(AR2, order = c(2,1,0))

##Stationory Series(2,0,3) ##Run ARIMA Model
cat("\014")
AR3<-arima.sim(n=100000, list(ar=c(0.2,0.3), ma=c(-0.1,-0.2,0.1)), innov=rnorm(100000))
arima(AR3, order = c(2,0,3))

