library(caTools)
library(glmnet)
data(swiss)
head (swiss, 4)
swissData <- data.frame (swiss)
dim(swissData)

set.seed(100) # set seed to replicate results
split<-sample.split(swissData$Infant.Mortality, SplitRatio=0.8)
train_set<-subset(swissData, split==TRUE)
test_set<-subset(swissData, split==FALSE)

dim(train_set)
dim(test_set)

lnMod<-lm(Infant.Mortality~., data = train_set)
summary(lnMod)
vif(lnMod)

predicted <- predict(lnMod, newdata = test_set)  
compare <- cbind (actual=test_set$Infant.Mortality, predicted)
compare
mean (apply(compare, 1, min)/apply(compare, 1, max))

x <- model.matrix(Infant.Mortality~., train_set)[,-1]
y <- train_set$Infant.Mortality
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
compare2 <- cbind (actual=test_set$Infant.Mortality, predicted)
compare2
mean (apply(compare, 1, min)/apply(compare, 1, max))