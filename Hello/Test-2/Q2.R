install.packages("AER")
library(AER)
data("UKNonDurables")

dataset1<-UKNonDurables
View(dataset1)
plot.ts(dataset1)
class(dataset1)

adf.test(dataset1)
##Data is non stationary
adf.test(diff(log(dataset1)))
##Model is stationary

acf(diff(log(dataset1)))##Does not follow a sime wave pattern, q=0
pacf(diff(log(dataset1)))##p=1, does not follow any sine wave pattern


arima.fit<-arima(log(dataset1), order=c(1,0,0), seasonal = c(0,1,1), seasonal=TRUE)
arima.fit

auto.arima(log(dataset1))

fcast1<-forecast(arima.fit, h=25)
plot(fcast1)

##Validate
Acf(fcast1$residuals)





data("PepperPrice")

dataset2<-PepperPrice
dataset2<-as.data.frame(PepperPrice)



dataset3<-ts(dataset2$white)
plot.ts(dataset3)

dataset4<-ts(dataset2$black)
plot.ts(dataset4)


adf.test(dataset3)
adf.test(diff(dataset3, differences = 1))

adf.test(dataset4)
adf.test(diff(dataset4, differences = 1))


acf(diff(dataset3))##Does not follow a sime wave pattern, q=1
pacf(diff(dataset3))#p=0

arima.fit2<-arima(dataset3, order=c(1,1,0))
arima.fit2

arima.fit2<-arima(dataset3, order=c(0,1,1))
arima.fit2

arima.fit2<-arima(dataset3, order=c(0,1,1))##Best Model
arima.fit2

auto.arima(dataset3)

fcast2<-forecast(arima.fit2, h=5)
plot(fcast2)
##Validate
Acf(fcast2$residuals)




acf(diff(dataset4))##Does not follow a sime wave pattern, q=1
pacf(diff(dataset4))#p=2

arima.fit3<-arima(dataset4, order=c(1,1,0))
arima.fit3

arima.fit3<-arima(dataset4, order=c(1,1,1))
arima.fit3

arima.fit3<-arima(dataset4, order=c(2,1,1))##Best Model
arima.fit3

auto.arima(dataset4)

fcast3<-forecast(arima.fit3, h=25)
plot(fcast3)

##Validate
Acf(fcast3$residuals)