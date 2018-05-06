advDataset<-read.csv("Adv.csv", header = TRUE, stringsAsFactors = FALSE)
set.seed(1)
lm.fit<-lm(Sales~TV+Radio+TV+Newspaper, data=advDataset)
summary(lm.fit)

glm.fit=glm(Sales~(TV+Radio+Newspaper),data=advDataset)
error=cv.glm(advDataset,glm.fit,K=5)$delta[1]
error

glm.fit=glm(Sales~(TV*Radio),data=advDataset)
error=cv.glm(advDataset,glm.fit,K=5)$delta[1]
error

glm.fit=glm(Sales~(TV*Newspaper),data=advDataset)
error=cv.glm(advDataset,glm.fit,K=5)$delta[1]
error

glm.fit=glm(Sales~(Radio*Newspaper),data=advDataset)
error=cv.glm(advDataset,glm.fit,K=5)$delta[1]
error


