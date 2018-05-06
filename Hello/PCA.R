library(fpp)
library(devtools)
library(forecast)
library(TTR)
library(tseries)
library(caTools)
library(FactoMineR)
library(stats)

m<-matrix(c(3.69,1.6,1.69,1.125), nrow = 2, ncol = 2)
Eigenvalues <- eigen(m)$values
Eigenvalues

Eigenvectors <- eigen(m)$vectors
Eigenvectors
sum(Eigenvalues)


setwd("C:\\F\\NMIMS\\DataScience\\Sem-2\\DS\\PCA")
carData_pca <- read.csv("cars.csv")
head(carData_pca)
carData_pca<-carData_pca[,5:14]
View(carData_pca)
carData_pca<-na.omit(carData_pca)


Eigenvalues <- eigen(cor(carData_pca))$values
Eigenvalues
Eigenv <- eigen(cor(carData_pca))$vectors
Eigenv


cat("\014")
scaledData<-scale(carData_pca)
pca2 <- prcomp(scaledData)
varimax(pca2$rotation)


pca.var <- pca2$sdev^2  ##which are identical to the Eigenvalues
plot(pca2)

pca4 <- princomp(scaledData)
pca4
names(pca4)
unclass(pca4$loadings)

pca3 = PCA(scaledData, graph = FALSE)
names(pca3)
pca3$eig
