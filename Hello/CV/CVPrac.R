x<-read.csv("Adv.csv")

set.seed(17)
glm.fit=glm(Sales~TV,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

glm.fit=glm(Sales~Radio,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

glm.fit=glm(Sales~Newspaper,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error


glm.fit=glm(Sales~TV+Radio+Newspaper,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

glm.fit=glm(Sales~Radio+Newspaper+Radio*Newspaper,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

glm.fit=glm(Sales~TV+Newspaper+TV*Newspaper,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

glm.fit=glm(Sales~TV+Radio+TV*Radio,data=x)
cv.error=cv.glm(x,glm.fit,K=5)$delta[1]
cv.error

