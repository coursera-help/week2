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
library(leaps)  ##performs best sub selection
regfit.full=regsubsets(Salary~.,Hitters)
?regsubsets
summary(regfit.full)
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
##But the nvmax option can be used
##in order to return as many variables as are desired. Here we fit up to a
##19-variable model
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq  ##R2 statistic increases from 32%,
##when only one variable is included in the model,
##to almost 55 %, when all variables are included
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

?plot.regsubsets
##command which can be used to display the selected variables for the best model with a given
##number of predictors, ranked according to the BIC, Cp, adjusted R2, or AIC
par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full, 6) ## default BIC


#################### Forward Stepwise Selection ############

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
coef(regfit.fwd,7)
#################### Backward Stepwise Selection ############
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.bwd,7)

# Choosing Among Models
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
##Now we run a loop, and for each size i, we
##extract the coefficients from regfit.best for the best model of that size,
##multiply them into the appropriate columns of the test model matrix to
##form the predictions, and compute the test MSE.
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)

##cross Validation
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit =regsubsets (Salary~.,data=Hitters [folds !=j,],nvmax =19)
  for(i in 1:19) {
    pred=predict(best.fit,Hitters[folds ==j,], id=i)
    cv.errors [j,i]=mean((Hitters$Salary[folds ==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


# Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]  ##qualitative variable to quantitative
x
y=Hitters$Salary

# Ridge Regression

library(glmnet)
##package in order to perform ridge regression
##function standardizes the variables so that they are on the same scale
grid=10^seq(10,-2,length=100)
##range of ?? values ?? = 10^10 to ?? = 10^???2,
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) ##If alpha=0 then a ridge regression model is fit, and if alpha=1 then a lasso model is fit
dim(coef(ridge.mod)) ##with 20 rows (one for each predictor, plus an intercept) and 100 columns (one for each value of ??)
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) ##l2 norm
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))  ####l2 norm
predict(ridge.mod,s=50,type="coefficients")[1:20,]

##We now split the samples into a training set and a test set in order
##to estimate the test error of ridge regression and the lasso
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) ##fit a ridge regression model on the training set, and evaluate
##its MSE on the test set, using ?? = 4
mean((ridge.pred-y.test)^2)   ##MSE
mean((mean(y[train])-y.test)^2)  ##MSE
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])  ##using ?? = 10^10
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)  ####using ?? = 0
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
##cross-validation
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0)  ##all data set
predict(out,type="coefficients",s=bestlam)[1:20,]
##none of the coefficients are zero-ridge regression does not
##perform variable selection!

###### The Lasso ##################################
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]