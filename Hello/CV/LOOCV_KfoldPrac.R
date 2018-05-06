############# LOOCV ############################
setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\CrossValidation\\data")
library(caTools)
library(Metrics)
library(boot)
advData<-read.csv("Adv.csv", header = TRUE, stringsAsFactors = FALSE)
head(advData)
advData<-advData[,-1]

fit<-lm(Sales~., data=advData)
summary(fit)

cat("\014")
glm.fit=glm(Sales~(TV+Radio+TV*Radio), data=advData)
cv.err=cv.glm(advData,glm.fit)
cv.err$delta



####### k-Fold Cross-Validation ############
cat("\014")
set.seed(17)
cv.error.10=rep(0,10)
for (i in 10){
  glm.fit=glm(Sales~poly(TV+Radio+TV*Radio, i), data=advData)
  cv.error.10[i]=cv.glm(advData,glm.fit,K=10)$delta[1]
}
cv.error.10





controlParams<-trainControl(method="cv", number=5, savePredictions = TRUE)
modelTrain<-train(Sales~TV+Radio+TV*Radio, data=trainSet, method="lm", trControl=controlParams)



