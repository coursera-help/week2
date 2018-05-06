library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)
library(dplyr)

setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\ML-R")

#Read CSV
dataset1 <- read.csv("./data/mw1.csv", header = T, stringsAsFactors = F)
dataset2 <- read.csv("./data/mw2.csv", header = T, stringsAsFactors = F)

nrow(dataset1)
nrow(dataset2)

#dataset21<-dataset2[,2:5]

################################## MEAN RELATIVE HUMIDITY ####################
dataset21$.RH[dataset21$.RH==""]<-NA
dataset21<-na.omit(dataset21)
#dataset21<-complete.cases(dataset21)


datasetHumid <- ts(dataset21$.RH, start = c(1984,1), end=c(2008,12) , frequency=12)


View(dataset21)
plot.ts(datasetHumid)
decompose(datasetHumid)

trainH<-window(datasetHumid, end=1999.99)
testH<-window(datasetHumid, start=2001)

cat("\014")
ets(trainH)
f11<-hw(trainH,seasonal="additive", damped=TRUE, h=108)
f11
accuracy(f11, testH)

f12<-hw(trainH,seasonal="multiplicative", exponential =TRUE, h=108)
accuracy(f12, testH)


f13<-hw(trainH,seasonal="additive",  h=108)
accuracy(f13, testH)
### This the best Model

cat("\014")
f12<-hw(testH, seasonal="additive", h=12)
f12
plot(f12)





############################ TOTAL RAINFALL IN THE MONTH ####################################################
dataset1 <- read.csv("./data/mw1.csv", header = T, stringsAsFactors = F)
View(dataset1)
dataset1$RD[dataset1$RD==""]<-NA
dataset1<-na.omit(dataset1)




datasetRainfall <- ts(dataset1$TMRF, start = c(1984,1), end=c(2008,12) , frequency=12)
plot.ts(datasetRainfall)
decompose(datasetRainfall)
seasonplot(datasetRainfall, col=rainbow(10))
class(datasetRainfall)

trainRD<-window(datasetRainfall, end=1999.99)
testRD<-window(datasetRainfall, start=2001)

cat("\014")
#ets(trainH)
f11<-hw(trainRD, seasonal="additive", damped=TRUE, h=108)
accuracy(f11, testRD)

cat("\014")
f13<-hw(trainRD,seasonal="additive",  h=108)
accuracy(f13, testRD)
### This the best Model

cat("\014")
f12<-hw(testRD, seasonal="additive", h=12)
f12
plot(f12)

############################# MEAN MAXIMUM TEMPERATURE ################################################


dataset1$.MMAX[dataset1$.MMAX==""]<-NA
dataset1<-na.omit(dataset1)


datasetTemp <- ts(dataset1$.MMAX, start = c(1984,1), end=c(2008,12) , frequency=12)
plot.ts(datasetTemp)
decompose(datasetTemp)
seasonplot(datasetTemp, col=rainbow(10))
class(datasetTemp)

trainTemp<-window(datasetRainfall, end=1999.99)
testTemp<-window(datasetRainfall, start=2001)

cat("\014")
#ets(trainH)
f11<-hw(trainTemp,seasonal="additive", damped=TRUE, h=108)
accuracy(f11, testTemp)

f12<-hw(trainTemp,seasonal="multiplicative",  h=108)
accuracy(f12, testTemp)


cat("\014")
f13<-hw(trainTemp,seasonal="additive",  h=108)
accuracy(f13, testTemp)
### This the best Model

cat("\014")
f12<-hw(testTemp, seasonal="additive", h=12)
f12
plot(f12)

########################### MEAN WIND SPEED ################################################

dataset2 <- read.csv("./data/mw2.csv", header = T, stringsAsFactors = F)

dataset2$.MWS[dataset2$.MWS==""]<-NA
dataset2<-na.omit(dataset2)
#dataset21<-complete.cases(dataset21)


datasetWindSpeed <- ts(dataset2$.MWS, start = c(1984,1), end=c(2008,12) , frequency=12)



plot.ts(datasetWindSpeed)
decompose(datasetWindSpeed)

trainWind<-window(datasetWindSpeed, end=1999.99)
testWind<-window(datasetWindSpeed, start=2001)

cat("\014")
ets(trainH)
f11<-hw(trainWind,seasonal="additive", damped=TRUE, h=108)
accuracy(f11, testWind)
### This the best Model

f12<-hw(trainWind,seasonal="multiplicative", exponential =TRUE, h=108)
accuracy(f12, testWind)


f13<-hw(trainWind,seasonal="additive",  h=108)
accuracy(f13, testWind)


cat("\014")
f12<-hw(testWind, seasonal="additive", damped=TRUE, h=12)
f12
plot(f12)

########################### Humidity 3 hour ################################################

datasetHumid3 <- read.csv("./data/mw3Humid.csv", header = T, stringsAsFactors = F)

datasetHumid3$.RH[datasetHumid3$.RH==""]<-NA
datasetHumid3<-na.omit(datasetHumid3)
#dataset21<-complete.cases(dataset21)


datasetHumid3hr <- ts(datasetHumid3$.RH, start = c(1984,1), end=c(2008,12) , frequency=12)



plot.ts(datasetHumid3hr)
decompose(datasetHumid3hr)

trainHumid3<-window(datasetHumid3hr, end=1999.99)
testHumid3<-window(datasetHumid3hr, start=2001)

cat("\014")
#ets(trainH)
f11<-hw(trainHumid3,seasonal="additive", damped=TRUE, h=108)
accuracy(f11, testHumid3)
#This is the best Model

f12<-hw(trainHumid3,seasonal="multiplicative", exponential =TRUE, h=108)
accuracy(f12, testHumid3)


f13<-hw(trainHumid3,seasonal="additive",  h=108)
accuracy(f13, testHumid3)


cat("\014")
f12<-hw(testHumid3, seasonal="additive", damped=TRUE, h=12)
f12
plot(f12)


########################### Humidity 12 hour ################################################

datasetHumid12 <- read.csv("./data/mw12Humid.csv", header = T, stringsAsFactors = F)

datasetHumid12$.RH[datasetHumid12$.RH==""]<-NA
datasetHumid12<-na.omit(datasetHumid12)
#dataset21<-complete.cases(dataset21)


datasetHumid12hr <- ts(datasetHumid12$.RH, start = c(1984,1), end=c(2008,12) , frequency=12)



plot.ts(datasetHumid12hr)
decompose(datasetHumid12hr)

trainHumid12<-window(datasetHumid12hr, end=1999.99)
testHumid12<-window(datasetHumid12hr, start=2001)

cat("\014")
#ets(trainH)
f11<-hw(trainHumid12,seasonal="additive", damped=TRUE, h=108)
accuracy(f11, testHumid12)


f12<-hw(trainHumid12,seasonal="multiplicative", exponential =TRUE, h=108)
accuracy(f12, testHumid12)
#This is the best Model

f13<-hw(trainHumid12,seasonal="additive",  h=108)
accuracy(f13, testHumid12)

f14<-hw(trainHumid12,seasonal="multiplicative", damped  =TRUE, h=108)
accuracy(f14, testHumid12)

f15<-hw(trainHumid12,seasonal="multiplicative", damped  =TRUE, exponential =TRUE, h=108)
accuracy(f15, testHumid12)

cat("\014")
f12<-hw(testHumid12, seasonal="multiplicative", exponential =TRUE, h=12)
f12
plot(f12)
