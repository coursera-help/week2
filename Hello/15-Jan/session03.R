library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)




# Simple Exponential Smoothing, alpha=0.5
beer<-aggregate(ausbeer)
beertrain<-window(beer, end=1999.99)
beertest<-window(beer, start=2000)
f5<-ses(beertrain, alpha=0.5, initial="simple", h=8)
f5
plot(f5)
accuracy(f5)

f6<-ses(beertrain, initial="simple", h=8)
f6
f6$model
accuracy(f6)
plot(residuals(f6))
checkresiduals(f6)

f5<-ses(ausbeer, alpha=0.5, initial="simple", h=8)
f5



View(taylor)
plot(taylor)
decompose(taylor)
str(decompose(taylor))
head(decompose(taylor))


View(a10)
plot(a10)
agg10 <- aggregate(a10)
agg10train<-window(agg10, end=1999.99)
agg10test<-window(agg10, start=2000)
f7<-ses(agg10train, alpha=0.2, initial="simple", h=8)
f7
f7<-ses(agg10train, initial="simple", h=8)
f7
f7$model
plot(f7)


a10train<-window(a10, end=1996)
a10test<-window(a10, start=1997)
f7<-ses(a10train, initial="simple", h=100)
f7
f7$model
plot(f7)




