data('iris')
iris
summary(iris)

library(rpart)
maxtree=rpart(Species~.,data=iris, minsplit=2 , cp=0)
maxtree
help(printcp)
A=printcp(maxtree, 3)
A
#nsplit =  leaves- 1
#root error: error on learning set, useless it's like what happens if you do nothing
#rel error : error on the learning sample of the tree -> 0
#xerror: cv  error
#cp=alphak della lezione [0.5 0.44 0.02 0.01 0.005 0]
CP=A[,1]
tree=c()
Lengthcp=length(CP)
Lengthcp
CP
for (k in 2 : Lengthcp) {
  x11()
  R=prune(maxtree,cp=CP[k])
  tree=c(maxtree, list(R))
  plot(R)
  text(R)
}

graphic.off()

t1=prune(maxtree, 0.5)
t2=prune(maxtree, 0.44)
t3=prune(maxtree, 0.02)
t4=prune(maxtree, 0.01)  #critalpha = R(A) + alpha * card(A)/
t5=prune(maxtree, 0.005)
plotcp(maxtree)

#se1 threshold dotted line
########################################################
library(rpart)
library(MASS)
data(fgl)
fgl
summary(fgl)
#=sample(fgl, 200)
#learn$splt=sample.split(fgl,SplitRatio=200/214)

set.seed(2005); 
test.rows <- sample(1:nrow(fgl), 14); 
test.set <- fgl[test.rows, ]; 
train.set <- fgl[-test.rows, ]

maxtreefgl=rpart(type~.,data=train.set,minsplit=2, cp=0  )

plot(maxtreefgl)
text(maxtreefgl)
A=printcp(maxtreefgl, 3)

plotcp(maxtreefgl)
prediction= predict(maxtreefgl,test.set, type='class')
fgl[test.rows, 10]
prediction
error=1/length(maxtreefgl)*sum(fgl[test.rows, 10]!=prediction)
for(ii in 1:100){}
  #ripeti prima

########################################################
#threshold = mincverror+ std
myBestTree <- function(T) {
  res=printcp(T)
  thres=min(min(res[,4]) + res[which(res[,4] == min(res[,4])),5])
  k=which(res[,4] == min(res[,4]))
  B=1*(res[,4]<thres) ###trassformiamo da bool a numeri
  L=which.max(B)[1]
  bestT=prune(T,cp=res[L,1])
  return(bestT)
}


myBestTree(maxtreefgl)

####################################################################
#bagging
k=10;
bag<-function(dataset,k,m){
  n=nrow(dataset)
  u=sample(1:n,m,replace=FALSE)
  train=dataset[u,]
  test=dataset[-u,]
  P=matrix(data=0,nrow=k, ncol=m)
  Pb=P
  for(ii in 1:k){
    set.seed(2005) 
    trainrows<-sample(1:m, m,replace = TRUE);
    traink = train[u,]
    maxtrees = rpart(traink[,1]~., data=traink, minsplit=2, cp=0 )
    besttrees = myBestTree(maxtrees)
      if(is.factor(dataset[,1])) {  #### super careful on class - reg dipende dal tipo della variabile da predire
        P[ii,]= predict(maxtrees,test[,-1], type='class')
        Pb[ii,]= predict(besttrees,test[,-1], type='class')
        
    else #### super careful on class - reg dipende dal tipo della variabile da predire
      P[ii,]= predict(maxtrees,test[,-1])
      Pb[ii,]= predict(besttrees,test[,-1]}
     }
  error=apply(P, function(x) 1/length(m)*sum(test[, 1]!=P[x,])
}
  
set.seed(2005) 
test.rows <- sample(1:nrow(fgl), 14,replace = FALSE); 
test.set<- fgl[test.rows, ]; 
train.set <- fgl[-test.rows, ]
for(ii in 1:k){
  set.seed(2005) 
  trainrows<-sample(1:150, 150, replace = TRUE);
  traink = train.set[trainrows]
  maxtrees = rpart(type~., data=traink, minsplit=2, cp=0 )
  besttrees = myBestTree(maxtrees)
  prediction1= predict(maxtrees,test.set, type='class')
  errorMax[ii]=1/length(maxtrees)*sum(fgl[test.rows, 10]!=prediction1)
  prediction2= predict(besttrees,test.set, type='class')
  errorBest[ii]=1/length(maxtrees)*sum(fgl[test.rows, 10]!=prediction2)
}
