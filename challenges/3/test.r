library(ggplot2)
library(neuralnet)
library(boot)
library(caret)
library(nnet)
library(tictoc)
library(zoo)
library(lattice)
require(randomForest)

setwd("~/rk/personal/study/WiSe2016/AnomalyDetection/challenges/3")

write.file <- function(data, file, header, funct = write.table, ...){
  # create and open the file connection
  datafile <- file(file, open = 'wt')
  # close on exit
  on.exit(close(datafile))
  # if a header is defined, write it to the file (@CarlWitthoft's suggestion)
  if(!missing(header)) writeLines(header,con=datafile)
  # write the file using the defined function and required addition arguments  
  funct(data, datafile,quote=F,sep=",",col.names=F,...)
}

kd <- read.csv("training-set.csv", sep=",",header = TRUE, dec=".", quote="", stringsAsFactors= TRUE)
data<-kd[complete.cases(kd),]
data<-data[,-c(1,3,4,5,44)]
data$label<-as.factor(data$label)
data_test<-read.csv("testing-set.csv", sep=",",header = TRUE, dec=".", quote="", stringsAsFactors= TRUE)
data_test<-data_test[,-c(1,3,4,5)]
#levels(data$service)<-union(levels(data_test$service), levels(data$service))
#levels(data$state)<-union(levels(data_test$state), levels(data$state))
auc<-c(0,0,0,0,0)
for(i in 1:5)
{
  

index <- sample(1:nrow(data),round(0.8*nrow(data)))
train.cv <- data[index,]
test.cv <- data[-index,]

predict_random<-function(data){
  
  tr_control<-trainControl(method="cv", number=5, repeats=1,allowParallel = T)
  
  grid <- expand.grid(mtry =seq(5,40,8))
  rf.fit<- train(label ~ .,data=data,method="rf",trControl=tr_control,tuneGrid=grid,ntrees=200)
  #rf.fit <- train(fake ~ .,data=data,method="rf",ntrees=avi[i],mtry=20)
  #rf.fit <- train(fake ~ .,data=data,method="svmLinear",trControl=tr_control)
  #rf.fit<-randomForest(fake ~ .,data=data,ntree=68,mtry=4)
  rf.fit
}

#data.rose <- ROSE(label ~ ., data = train.cv, seed = 1)$data
num<-table(data$label)
data.rose <- ovun.sample(label ~ ., data = data, method = "under", N = 2*num[2], seed = 1)$data
#data.rose <- ovun.sample(label ~ ., data =train.cv, method = "both", p=0.2,N=15000, seed = 1)$data
#data.rose <- ovun.sample(label ~ ., data =train.cv, method = "under", N = 15000, seed = 1)$data
#avira<-randomForest(formula=label ~ .,data=data.rose,mtry=7,ntree=70)
#avira_dt<-rpart(label ~ .,data=data.rose)
avira<-randomForest(formula=label ~ .,data=data.rose,mtry=8,ntree=70)
avi<-predict(avira,test.cv)
#results<-predict(avira,data_test)
#acc<-accuracy.meas(test.cv$label, avi)
aucr<-roc.curve(test.cv$label, avi, plotit = F)
auc[i]<-aucr$auc

}
results<-mean(auc)
#data.rose <- ROSE(label ~ ., data = train.cv, seed = 1)$data
#avi<-predict_random(data)
#results<-predict(avi,data_test[,-c(1)])
#index<-seq(1,82332)
#avi<-data.frame(index,results)
#names(avi)<-c("Id","label")
#write.file(new_data,header = "Id,label",file = "D:\\others\\Network_Anomaly_Detection\\2nd_try.csv",row.names=FALSE)