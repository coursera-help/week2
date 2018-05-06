setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\Clustering")

library(MASS)
library(ISLR)
library(caret)
kData<-read.csv("kCluster.csv", header = TRUE, stringsAsFactors = FALSE)
head(kData)
dim(kData)

model<-kmeans(kData[1:2],3)
model


library(class)
knnData<-read.csv("KNN.csv", header = TRUE, stringsAsFactors = FALSE)
head(knnData)
dim(knnData)
nrow(knnData)
knnTrain<-knnData[1:24,1:2]
knnTest=knnData[25,1:2]

knnTrain1<-knnData[1:24,]

# Run k-NN:
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
knnFit <- train(Y ~ ., data = knnTrain1, method = "knn", trControl = ctrl, preProcess = c("center","scale"),tuneLength = 20)
knnFit

cat("\014")
model1 <- knn(train = knnTrain, test = knnTest, cl = knnData[1:24,]$Y, k=5)
model1



