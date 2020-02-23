##bagging
acc<-c()
for(i in 1:1000)
{
  print(i)
  #data division
  inTraininglocal<-caret::createDataPartition(iris$Species,p=.85,list = F)#data division
  training1<-iris[inTraininglocal,]#training the dataset
  testing<-iris[-inTraininglocal,]#testing the dataset
  #model/fittree building
  fittree<- C5.0(training1$Species~.,data=training1)#Trials-Bossting parameter
  #predict for test dataset
  pred<-predict.C5.0(fittree,testing[,-5])
  a<-table(testing$Species,pred)
  #accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
}
  #generate model summary
  summary(acc)
  boxplot(acc)
  
  