setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\28thFebLogistic")

#######################Trainining Data#####################
cat("\014")
trainData<-read.csv("trainSample.csv", header = TRUE, stringsAsFactors = FALSE)
dim(trainData)
head(trainData)


glm.fit<-glm(default~., data=trainData, family="binomial")
summary(glm.fit)

glm.fit<-glm(default~(age+ed+employ+address+income+dbtinc+creddebt), data=trainData, family="binomial")
summary(glm.fit)

glm.fit<-glm(default~(age+ed+employ+address+dbtinc+creddebt), data=trainData, family="binomial")
summary(glm.fit)

glm.fit<-glm(default~(age+employ+address+dbtinc+creddebt), data=trainData, family="binomial")
glm.fit
summary(glm.fit)


predicted<-predict(glm.fit, type="response")
conf<-table(trainData$default,predicted>0.5)
Accuracy<-sum(diag(conf))/sum(conf)

###########Test####################################
testData<-read.csv("testSample.csv", header = TRUE, stringsAsFactors = FALSE)
dim(testData)
head(testData)

glm.fit<-glm(default~(age+employ+address+dbtinc+creddebt), data=testData, family="binomial")
glm.fit
summary(glm.fit)

predicted2<-predict(glm.fit, type="response")
conf<-table(predicted=predicted2>0.5, actual=testData$default)
Accuracy=sum(diag(conf))/sum(conf)

###### ROCR Curve ######
library(ROCR)
ROCRpred <- prediction(predicted2, testData$default)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)
auc






##################################Take whole obs##########################
library(caTools)
library(glmnet)
library(MASS)
library(ISLR)
library(boot)
wholeData<-read.csv("wholeData.csv", header = TRUE, stringsAsFactors = FALSE)
wholeData$default<-as.factor(wholeData$default)
class(wholeData$default)
dim(wholeData)
head(wholeData)

set.seed(100) # set seed to replicate results
split<-sample.split(wholeData$default, SplitRatio=0.8)
train_set<-subset(wholeData, split==TRUE)
test_set<-subset(wholeData, split==FALSE)

dim(train_set)
dim(test_set)


##### K fold ###########################
library(caret)

train_control<- trainControl(method="cv", number=10)

model<- train(default~., data=wholeData, trControl=train_control, method="glm", family="binomial")

summary(model)

predTrain = predict(model, newdata=test_set) 
conf <- table(test_set$default, predTrain)
accuracy<-sum(diag(conf))/sum(conf)
accuracy



