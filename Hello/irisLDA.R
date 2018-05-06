setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\LDA")

########################## LDA #############################################
library(MASS)
irisdata<-read.csv("Iris.csv", header = TRUE, stringsAsFactors = FALSE)
head(irisdata)
tail(irisdata, 60)

irisdata$Class <- factor(irisdata$Class, labels = c(1,2,3))

ldModel<-lda(Class~., data=irisdata)
ldModel
summary(ldModel)

predictedVal<- predict(ldModel, newdata = irisdata)
predictedVal

Zval<-as.data.frame(predictedVal$x)
Z1<-mean(Zval$LD1)
Z2<-mean(Zval$LD2)




cbind(actuals=irisdata$Class, predictedVal$class)
conf<-table(irisdata$Class, predictedVal$class)
accuracy<-sum(diag(conf))/sum(conf)
accuracy