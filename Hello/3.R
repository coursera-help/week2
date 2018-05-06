View(Auto)

lm.fit<-lm(mpg~horsepower, data=Auto)
summary(lm.fit)

lm.fit2<-lm(mpg~poly(horsepower, 2), data=Auto)
summary(lm.fit2)

lm.fit3<-lm(mpg~poly(horsepower, 3), data=Auto)
summary(lm.fit3)
