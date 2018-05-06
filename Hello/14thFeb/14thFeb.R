time<-1:100
y<-arima.sim(n=100, list(ar=c(0.9)), innov=rnorm(100))
data=10+0.5*time+y
plot.ts(data)

m1<-lm(data~time)
summary(m1)
res1<-resid(m1)
adf.test(res1)

pacf(res1)
acf(res1)

arima.fit<-auto.arima(res1)
arima.fit

arima.fit1<-arima(res1, order=c(1,0,0))
arima.fit1

y<-predict(arima.fit1, n.ahead=5)
y$pred

x<-predict(m1)[101:105]
x

x+y$pred

##############################################
time<-1:1500
y1<-arima.sim(n=1500, list(ar=-0.5, ma=0.3), innov=rnorm(1500))
auto.arima(y1)
data=10+0.5*time+y1
data<-ts(data)

decompose(data)


m1<-lm(data~time)
res1<-resid(m1)
adf.test(res1)

pacf(res1)
acf(res1)

arima.fit<-arima(res1, order=c(1,0,1))
arima.fit

y<-predict(arima.fit, n.ahead = 5)
y$pred

x<-predict(m1)[101:105]
x

plot.ts(x+y$pred)

