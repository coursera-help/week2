library(MASS)
library(ISLR)
fix(Smarket)
names(Smarket)
dim(Smarket)
attach(Smarket)
######################## Logistic Model ##################
cat("\014")
lm.fit<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family="binomial")
lm.fit
summary(lm.fit)
coef(lm.fit)

pred.fit<-predict(lm.fit, type="response")
pred.fit
glm.pred=rep("Down",1250)
glm.pred[pred.fit>0.5]="Up"
table(glm.pred,Direction)
mean(glm.pred==Direction) ##Accuracy
conf<-table(pred.fit>0.5,Direction)
accuracy<-sum(diag(conf))/sum(conf)
accuracy

### Split the data into train and test and then fit Logistic Model
cat("\014")
train=(Year<2005)
test.2005=Smarket[!train,]
dim(test.2005)

lm.fit1<-glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family="binomial", subset=train)
lm.fit1
summary(lm.fit1)
pred.fit1<-predict(lm.fit1, newdata = test.2005, type="response")
Direction.2005=Direction[!train]
glm.pred1=rep("Down", 252)
glm.pred1[pred.fit1>0.5]="Up"
table(glm.pred1, Direction.2005)
mean(glm.pred1==Direction.2005)
conf<-table(glm.pred1, Direction.2005)
conf
accuracy<-sum(diag(conf))/sum(conf)
accuracy

lm.fit1<-glm(Direction~Lag1+Lag2, data=Smarket, family="binomial", subset=train)
lm.fit1
summary(lm.fit1)
pred.fit1<-predict(lm.fit1, newdata = test.2005, type="response")
Direction.2005=Direction[!train]
glm.pred1=rep("Down", 252)
glm.pred1[pred.fit1>0.5]="Up"
table(glm.pred1, Direction.2005)
mean(glm.pred1==Direction.2005)
mean(glm.pred1!=Direction.2005)


###### LDA ##############
cat("\014")
lda.fit<-lda(Direction~Lag1+Lag2, data = Smarket, subset=train)
lda.fit

lda.pred<-predict(lda.fit, newdata=test.2005, type="response")
table(lda.pred$class, Direction.2005)
mean(lda.pred$class==Direction.2005)

############# QDA ##########
cat("\014")
qda.fit<-qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

qda.pred<-predict(qda.fit, newdata=test.2005, type="response")
table(qda.pred$class, Direction.2005)
mean(qda.pred$class==Direction.2005)

######## test Split ############
library(caTools)
cat("\014")
split<-sample.split(Direction, SplitRatio = 0.8)
train_set<-subset(Smarket, split==TRUE)
test_set<-subset(Smarket, split==FALSE)
dim(train_set)
dim(test_set)

glm.fit<-glm(Direction~Lag1+Lag2, data=train_set, family = "binomial")
glm.fit
glm.pred<-predict(glm.fit, newdata = test_set, type="response")
glm.pred
glm.tab=rep("Down",250)
glm.tab[glm.pred>0.5]="Up"
table(predicted=glm.tab, Actual=test_set$Direction)
mean(glm.tab==test_set$Direction)


conf5<-table(glm.tab, test_set$Direction)
accuracy5<-sum(diag(conf5))/sum(conf5)
accuracy5