
#setwd("~/Downloads")
#  dataclas<-read.table("tp3_a18_clas_app.txt")
classifieur_qda<-function(dataclas)
{
  s=0
  for (i in 1:1000)
  {
  n<-nrow(dataclas)
  p<-ncol(dataclas)
  train<-sample(1:n,size = floor(2*n/3))
  dataclas.train<-dataclas[train,]
  dataclas.train.x<-dataclas.train[,-p]
  dataclas.train.y<-as.vector(dataclas.train[,p])
  
  dataclas.test<-dataclas[-train,]
  dataclas.test.x<-dataclas.test[,-p]
  dataclas.test.y<-as.vector(dataclas.test[,p])
  
  library(MASS)
  qda.model <- qda(y~.,data=dataclas.train)
  pred.qda<-predict(qda.model,newdata=dataclas.test)
  s<-s+(1-sum(diag(table(dataclas.test$y,pred.qda$class)))/67)
  }
  s<-s/1000
  print(s)
}
  