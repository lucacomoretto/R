#example1
esame <- read.csv("C:/Users/luca/Desktop/EITnice/statcomp/esame.txt", sep="")
summary(esame)
library(rpart)
is.na(esame)


set.seed(2005); 
test.rows <- sample(1:nrow(esame), 30); 
test.set <- esame[test.rows, ]; 
train.set <- esame[-test.rows, ]
maxtreeesame = rpart (maxO3~.,data=train.set,minsplit=1, cp=0 )
printcp(maxtreeesame, 3)
plotcp(maxtreeesame)

myBestTree <- function(T) {
  res=printcp(T)
  thres=min(min(res[,4]) + res[which(res[,4] == min(res[,4])),5])
  k=which(res[,4] == min(res[,4]))
  B=1*(res[,4]<thres) ###trassformiamo da bool a numeri
  L=which.max(B)[1]
  bestT=prune(T,cp=res[L,1])
  print(L)
  return(bestT)
}
best = myBestTree(maxtreeesame)
prediction= predict(best,test.set)
prediction[1]
test.set[,1]

error=1/length(test.set[,1])*sum((test.set[,1]-prediction)^2)
error
