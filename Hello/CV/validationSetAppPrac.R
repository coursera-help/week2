##################Validation Set Approac##########################
setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\CrossValidation\\data")
library(caTools)
library(Metrics)
advData<-read.csv("Adv.csv", header = TRUE, stringsAsFactors = FALSE)
head(advData)

split=sample.split(advData$Sales, SplitRatio = 0.8)
trainSet<-subset(advData, split==TRUE)
testSet<-subset(advData, split==FALSE)

fit<-lm(Sales~., data=trainSet)
summary(fit)

######################First########################
cat("\014")
set.seed(123)
fit1<-lm(Sales~(TV+Radio+TV*Radio), data=trainSet) ##Best Model
summary(fit1)
y1<-predict(fit1, newdata=testSet)
mse(testSet$Sales, y1)

fit2<-lm(Sales~poly(TV+Radio+TV*Radio), data=trainSet)
summary(fit2)
y2<-predict(fit2, newdata=testSet)
mse(testSet$Sales, y2)

#############################Second############################
set.seed(1)
fit1<-lm(Sales~(TV+Radio+TV*Radio), data=trainSet)
summary(fit1)
y1<-predict(fit1, newdata=testSet)
mse(testSet$Sales, y1)

fit2<-lm(Sales~poly(TV+Radio+TV*Radio), data=trainSet)
summary(fit2)
y2<-predict(fit2, newdata=testSet)
mse(testSet$Sales, y2)

#############################Third############################
set.seed(2)
fit1<-lm(Sales~(TV+Radio+TV*Radio), data=trainSet)
summary(fit1)
y1<-predict(fit1, newdata=testSet)
mse(testSet$Sales, y1)

fit2<-lm(Sales~poly(TV+Radio+TV*Radio), data=trainSet)
summary(fit2)
y2<-predict(fit2, newdata=testSet)
mse(testSet$Sales, y2)
