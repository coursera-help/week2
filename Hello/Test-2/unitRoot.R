library(dplyr)
library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)
library(ggplot2)

setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\TS\\data")
data<-read.csv("UnitRoot.csv", header = TRUE, stringsAsFactors = FALSE)

head(data)
#Detect NAs
detectNA<- function(x){
  return(sum(is.na(x)))
}
sapply(unitData, detectNA)

detectNull<- function(x){
  return(sum(x=="null"))
}
sapply(unitData, detectNull)

unitSeries1<-ts(data$FX, start=c(1996,1))
plot.ts(unitSeries1)
class(unitSeries1)

unitSeries2<-ts(data$FTSE100, start=c(1996,1))
plot.ts(unitSeries2)
class(unitSeries2)

adf.test(unitSeries1)
adf.test(unitSeries2)
