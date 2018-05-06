##logistic Regression
library(ISLR)
fix(Default)
names(Default)
par(mfrow=c(1,2))
boxplot(Default$income~Default$default)
boxplot(Default$balance~Default$default)
plot(Default1,Default$balance,col="red")

default1<-as.numeric(Default$default)
convert<- function(x){
  if(x=='Yes')
    return(1)
  else
    return(0)
}
default1<-sapply(Default$default, convert)

#ifelse(Default$default=='Yes', default1=1)


plot(Default$balance,default1)
lm.fit<-glm(default1~Default$balance)
summary(lm.fit)
abline(glm(default1~Default$balance))
lm.fit1<-glm(Default$default~Default$balance,family=binomial)
summary(lm.fit1)
lm.fit2<-glm(Default$default~Default$student,family=binomial)
lm.fit3<-glm(Default$default~Default$balance+Default$student+Default$income,family=binomial)
summary(lm.fit3)

lm.fit4<-glm(Default$default~Default$balance+Default$student,family=binomial)
lm.fit4
glm.probs=predict(lm.fit4,type="response")
table(Default$default,glm.probs>0.5)

heart

write.csv(Default,file="C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\28thFebLogistic\\defaultDataset.csv")

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket,col=Smarket$Direction)  
plot(Smarket$Today, Smarket$Direction)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
##The diagonal elements of the confusion matrix indicate correct predictions,
##while the off-diagonals represent incorrect predictions
(507+145)/1250
mean(glm.pred==Direction)


train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
##small model
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)