##Holt Winter
library(tseries)
library(fpp)
library(devtools)
library(caTools)

View(debitcards)
plot.ts(debitcards)

trainDebit<-window(debitcards, end=2009.99)
testDebit<-window(debitcards, start=2011)

debitCardsforecasts<-hw(trainDebit, seasonal = "multiplicative", h=50)
debitCardsforecasts
debitCardsforecasts$model
plot(debitCardsforecasts, shaded=TRUE, shadecols="oldstyle")


##check for validation
accuracy(debitCardsforecasts, testDebit)
Acf(debitCardsforecasts$residuals, lag.max=20)
plot.ts(debitCardsforecasts$residuals)
#hist(debitCardsforecasts$residuals)
Box.test(debitCardsforecasts$residuals, lag=20, type="Ljung-Box")
##p-value is not significant, the residuals doesn't exhibit any correlation.