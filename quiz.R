x <- read.csv("diabetes.csv")
dim(x)
x <- na.omit(x)
head(x)
set.seed(123)
library(caTools)
split <- sample.split(x,SplitRatio = 0.8)
train <- subset(x,split=TRUE)
x_train <- train[,-ncol(x)]
y_train <- train[,ncol(x)]
test <- subset(x,split=FALSE)
x_test <- train[,-ncol(x)]
y_test <- train[,ncol(x)]
x$Outcome <- as.factor(x$Outcome)
logistic <- glm(train$Outcome~.,family = 'binomial',data=train)
summary(logistic)
prob <- predict(logistic,data=x_test,type = 'response')
head(prob)
pred <- ifelse(prob>0.5,1,0)
cm <- table(pred,y_test)
mean(pred==y_test)

library(caret)
log<-train(Outcome ~ .,data=train,method='glm',trControl=trainControl(method = 'cv'))
prob1 <- predict(log,data=x_test)
pred1 <- ifelse(prob1>0.5,1,0)
cm1 <- table(pred1,y_test)
mean(pred1==y_test)

library(MASS)
lda <- lda(train$Outcome~.,data=train)
summary(lda)
prob <- predict(lda,data=x_test)

cm1 <- table(prob$class,y_test)
mean(prob$class==y_test)

library(glmnet)
x.train <- model.matrix(Outcome~.,train)[,-1]
y.train <- train$Outcome

x.test <- model.matrix(Outcome~.,test)[,-1]
y.test <- test$Outcome

cv.out <- cv.glmnet(x.train,y.train,alpha=0,family="binomial",type.measure="mse")
plot(cv.out)

ridge <- predict(cv.out,newx=x.test,s=cv.out$lambda.1se,type='response')
pr <- ifelse(ridge>0.5,1,0)
table(pr,y_test)
mean(pr==y_test)


scaledData<-scale(carData_pca)
pca2 <- prcomp(scaledData)
varimax(pca2$rotation)
summary(pca2)