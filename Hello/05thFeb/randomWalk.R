
error<-rnorm(500)
mean(error)
plot.ts(error)
hist(error)


nifty<-rnorm(500)
nifty21<-rnorm(501:1000)
mean(nifty)
var(nifty)
plot.ts(nifty, col="red")

##RW non stationary
nifty1<-cumsum(nifty) ##Y0=0 and sum of random error
plot.ts(nifty1, col="blue")

##RW non stationary, Initial value is 1000
nifty2<-1000+cumsum(nifty)
plot.ts(nifty2)

##RW stationary
diffNifty<-diff(nifty2, differences=1)
plot.ts(diffNifty)


##RW with drift(alpha) non stationary
nifty2<-0.1+100+cumsum(nifty) #Y0=100, alpha=0.1
plot.ts(nifty2, col="blue")


##RW with drift(alpha) stationary
#nifty5<-0.1+100+cumsum(nifty) #Y0=100, alpha=0.1
diffNifty1<-diff(nifty2, differences=1)
plot.ts(diffNifty1)

##Deteministic Trend without drift non stationary
time<-1:5000
nifty3<-10+0.5*time+nifty1 #Y0=10, beta=0.5
plot.ts(nifty3, col="blue")

##Deteministic Trend without drift stationary
time<-1:500
nifty6<-10+0.5*time+nifty1  #Y0=10, beta=0.5
diffNifty2<-diff(nifty6, differences=1)
plot.ts(diffNifty2, col="blue")


##RW with Deteministic Trend with drift non staionary
time<-1:500
nifty4<-10+0.1+0.5*time+nifty1 #Y0=10, alpha=0.1, beta=0.5
plot.ts(nifty4, col="blue")

##RW with Deteministic Trend with drift stationary
time<-1:500
nifty7<-10+0.1+0.5*time+nifty1 #Y0=10, alpha=0.1, beta=0.5
diffNifty3<-diff(nifty6, 1)
plot.ts(diffNifty3, col="blue")



##When two stationary series
nifty2<-1000+cumsum(nifty)
nifty22<-10+cumsum(nifty21)

mean(nifty2)
mean(nifty22) 

var(nifty2)
var(nifty22)

nifty2M<-as.matrix(nifty2)
cov(x=nifty2M, y=NULL, method="pearson")

nifty22M<-as.matrix(nifty22)
cov(x=nifty22M, y=NULL, method="pearson")

modelNifty<-lm(nifty2~nifty22)
summary(modelNifty)

##Taking Diff
diffNifty23<-diff(nifty22, differences=1)
diffNifty<-diff(nifty2, differences=1)

mean(diffNifty23)
mean(diffNifty)

var(diffNifty23)
var(diffNifty)

diffNifty23M<-as.matrix(diffNifty23)
cov(x=diffNifty23M, y=NULL, method="pearson")

diffNiftyM<-as.matrix(diffNifty)
cov(x=diffNiftyM, y=NULL, method="pearson")

modelNifty1<-lm(diffNifty~diffNifty23)
summary(modelNifty1)

