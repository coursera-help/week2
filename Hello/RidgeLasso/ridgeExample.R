library(car)
library(ridge)
library(caTools)
data(longley, package="datasets")
head (longley, 4)
inputData <- data.frame (longley) # plug in your data here
colnames(inputData)[1] <- "response"

##Calculate Correlation
XVars <- inputData[, -1]
round(cor(XVars), 2)

set.seed(100) # set seed to replicate results
split<-sample.split(inputData$response, SplitRatio=0.8)
train_set<-subset(inputData, split==TRUE)
test_set<-subset(inputData, split==FALSE)

dim(inputData)
dim(train_set)
dim(test_set)

lmMod <- lm(response ~ ., data=train_set)  # the linear reg model
summary (lmMod) # get summary
vif(lmMod) # get VIF

predicted <- predict (lmMod, newdata = test_set)  # predict on test data
compare <- cbind (actual=test_set$response, predicted)
mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy


linRidgeMod <- linearRidge(response ~ ., data = train_set)
summary(linRidgeMod)
predicted2 <- predict(linRidgeMod, newdata = test_set)  # predict on test data
compare2 <- cbind (actual=test_set$response, predicted2)
mean (apply(compare2, 1, min)/apply(compare2, 1, max)) # calculate accuracy
