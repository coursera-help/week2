library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)


#beer<-aggregate(ausbeer)
beertrain<-window(ausbeer, end=1999.99)
beertest<-window(ausbeer, start=2000)

#Holt Linear Model(Additive)
f5 <- holt(beertrain, alpha=0.8, beta=0.2, initial="simple", h=8)
f5
f5$model
accuracy(f5, beertest)

#Holt Exponential Trend Model(Multiplicative)
f6 <- holt(beertrain, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=8)
f6
f6$model
accuracy(f6, beertest)

#Holt Linear Damped(Additive)
f7<-holt(beertrain, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=8)
f7
f7$model
accuracy(f7, beertest)

#Holt Exponential damped trend(Multiplicative)
f8<-holt(beertrain, alpha=0.8, beta=0.2, damped=TRUE, initial="simple",exponential=TRUE, h=8)
f8
accuracy(f8, beertest)




beertrain1<-window(ausbeer, end=2007)
beertest2<-window(ausbeer, start=2008)

##Holt winter seasonal additive model
f9<-hw(beertrain1, seasonal="additive", h=8)
f9
f9$model
accuracy(f9, beertest2)

####Holt winter seasonal multiplicative model
f10<-hw(ausbeer,seasonal="multiplicative", h=8)
f10
f10$model
accuracy(f10)

##Holt winter seasonal additive damped model
f11<-hw(ausbeer,seasonal="additive", damped=TRUE, h=8)
f11
f11$model


####Holt winter seasonal multiplicative damped model
f12<-hw(ausbeer,seasonal="multiplicative", damped=TRUE, h=8)
f12
f12$model

##Holt winter seasonal multiplicative exponential model
f13<-hw(ausbeer,seasonal="multiplicative", exponential=TRUE, h=8)
f13
f13$model

##Holt winter seasonal multiplicative exponential damped model
f14<-hw(ausbeer,seasonal="multiplicative", damped=TRUE, exponential=TRUE, h=8)
f14
f14$model