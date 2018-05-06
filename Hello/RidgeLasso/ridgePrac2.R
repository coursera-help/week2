library(MASS)
library(ISLR)
library(caTools)
library(glmnet)
data(Credit)
head (Credit, 4)
creditData <- data.frame(Credit)
dim(creditData)

set.seed(100) # set seed to replicate results
split<-sample.split(creditData$Balance, SplitRatio=0.8)
train_set<-subset(creditData, split==TRUE)
test_set<-subset(creditData, split==FALSE)

dim(train_set)
dim(test_set)

lnMod<-lm(Balance~., data = train_set)
summary(lnMod)
vif(lnMod)

predicted <- predict(lnMod, newdata = test_set)  
compare <- cbind (actual=test_set$Balance, predicted)
compare
mean((predicted-test_set$Balance)^2)


x <- model.matrix(Balance~., train_set)[,-1]
y <- train_set$Balance
ridge.mod <- glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
ridge.mod$lambda[100] ##lamda is ver small
coef(ridge.mod)[,100]

ridge.mod$lambda[1]
coef(ridge.mod)[,1]

##Choose best value of lamda
set.seed(1)
cv.out <- cv.glmnet(x, y, alpha=0, nlambda=100, lambda.min.ratio=0.0001)
plot(cv.out)

best.lambda <- cv.out$lambda.min
best.lambda

predicted2<- predict(ridge.mod, s=best.lambda, newx = test_set)
predicted2
compare2 <- cbind (actual=test_set$Balance, predicted)
compare2
mean((predicted2-test_set$Balance)^2)