library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)
cat("\014")

plot.ts(ausbeer)
decompose(ausbeer)
plot(decompose(ausbeer))


beer<-aggregate(ausbeer)
View(beer)
plot.ts(beer)

beertrain<-window(beer, end=1999.99)
beertest<-window(beer, start=2000)

#Mean Method
f1<-meanf(beertrain,h=8)
f1
plot(f1)
accuracy(f1, beertest)
attributes(f1)

#Naive Method
f2<-naive(beertrain, h=8)
f2
plot(f2)


#Moving Average
f3<-SMA(beertrain, n=3)%>%
forecast(h=8)%>%
plot()

f3<-SMA(beertrain, 3)
f4<-forecast(f3, 8)
plot(f4)

accuracy(f4, beertest)