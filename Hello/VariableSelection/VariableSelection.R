library(MASS)
library(ISLR)
# Best Subset Selection
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)  ##function removes all of the rows that have missing values in any variable.
dim(Hitters)
sum(is.na(Hitters))


library(leaps)  

##### Best Sub Selection
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)

reg.summary$rsq

par(mfrow=c(1,1))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")

### Max Adj R2
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

###### Min Cp
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)

##### Min BIC
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)


par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full, 6) ## default BIC


#################### Forward Stepwise Selection ############

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
plot(regfit.fwd, scale="Cp")
summary(regfit.fwd)$cp
coef(regfit.fwd, which.min(summary(regfit.fwd)$cp))

#################### Backward Stepwise Selection ############

regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
plot(regfit.bwd, scale="bic")
summary(regfit.bwd)$bic
coef(regfit.fwd, which.min(summary(regfit.bwd)$bic))

############### Validation Set Approach
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~., data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~., data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best, 10)

## K fold Cross Validation
predict.regsubsets = function(object, newdata, id) {
  form  <-  as.formula(~.)
  mat  <-  model.matrix(form, newdata)
  coefi  <-  coef(object, id)
  xvars  <-  names(coefi)
  mat[, xvars] %*% coefi
}

k=10
set.seed(1)

folds<-sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit =regsubsets (Salary~.,data=Hitters [folds !=j,],nvmax =19)
  for(i in 1:19) {
    pred=predict.regsubsets(best.fit,Hitters[folds ==j,], id=i)
    cv.errors [j,i]=mean( (Hitters$Salary[folds ==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)