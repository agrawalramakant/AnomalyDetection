library(lattice)
library(caret)
require(randomForest)

#setwd("D:\\others\\AnomalyDetection-master\\challenges\\2\\challenge_data_test.tar\\challenge_data_test")
data_test<-read.csv("test_fakenes.csv", sep=",",header = FALSE,dec = ".")
data_test<-data_test[,-c(1,5)]
#data_test<-data_test[complete.cases(data_test),]
data_test1<-read.delim("yelp_data_reviewer.dat",header = TRUE,sep=";",dec=".",stringsAsFactors = FALSE)
data_test2<-read.delim("yelp_data_hotel.dat",header = TRUE,sep=";",dec=".",stringsAsFactors = FALSE)
names(data_test)<-c("date","reviewID","reviewerID","rating","usefulCount","coolCount","funnyCount","hotelID","Fakeness","Rev_len")


temp_data_test<-data.frame(0,0,0,0,0,0,0,0,0,0,0,0)
temp1_data_test<-data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13)
rows_invalid<-c()
rows_invalid_new<-c()
names(temp_data_test)<-c("reviewerid","location","joindate","friend_count_rev","reviewcount","firstcount","usefulcount","coolcount","funnycount","complimentcount","tipcount","fancount")
names(temp1_data_test)<-c("hotelID","name", "location","reviewCount","rating","categories","address","AcceptsCreditCards","PriceRange","WiFi", "webSite", "phoneNumber", "filReviewCount")
for(i in 1:nrow(data_test))
{
  temp<-data_test$reviewerID[i]
  temp1<-data_test$hotelID[i]
  # avira<-data_test1$reviewerID==temp
  #new<-avira==TRUE
  avi<-data_test1[data_test1$reviewerID==temp,c(1,3,4,5:13)]
  avi1<-data_test2[data_test2$hotelID==temp1,]
  if(nrow(avi)==0)
  {
    avi<-data_test1[12,c(1,3,4,5:13)]
    rows_invalid<-rbind(rows_invalid,i)
  }
  if(nrow(avi1)==0)
  {
    rows_invalid_new<-rbind(rows_invalid_new,i)
  }
  avi<-as.data.frame(avi,row.names = NULL)
  avi1<-as.data.frame(avi1,row.names = NULL)
  names(avi)<-c("reviewerid","location","joindate","friend_count_rev","reviewcount","firstcount","usefulcount","coolcount","funnycount","complimentcount","tipcount","fancount")
  names(avi1)<-c("hotelID","name", "location","reviewCount","rating","categories","address","AcceptsCreditCards","PriceRange","WiFi", "webSite", "phoneNumber", "filReviewCount")
  temp_data_test<-rbind(temp_data_test,avi)
  temp1_data_test<-rbind(temp1_data_test,avi1)
  
  
}

temp_data_test<-temp_data_test[2:nrow(temp_data_test),]
temp1_data_test<-temp1_data_test[2:nrow(temp1_data_test),]
#temp1_data_test<-temp1_data_test[-rows_invalid,]
#rows_invalid_new<-rbind(rows_invalid_new,rows_invalid)
#data_test<-data_test[-rows_invalid_new,]
data_test<-cbind(data_test,temp_data_test,temp1_data_test)
data_test[,c(11,23)]<-NULL


temp3<-c(1,2,3,8,11,12,22,23,26:33)
for(i in 1:length(temp3))
{
  data_test[,temp3[i]]<-as.factor(data_test[,temp3[i]])
}
datanew_test<-data_test[,-c(1,2,3,8,11,12,22,23,27,31,32)]
#data_testnew<-data_test[,-c(1,2,3,9,10,11)]
#data_testnew<-data_testnew[,-c(6)]

datanew_test[,7]<-as.numeric(datanew_test[,7])
predict_random<-function(data_test,ntrees){
  
  tr_control<-trainControl(method="cv", number=5, repeats=1,allowParallel = T)
  grid <- expand.grid(mtry = seq(4,15,3))
  rf.fit <- train(fake ~ .,data=data_test,method="rf",tuneGrid=grid,trControl=tr_control,ntrees=ntrees)
  
  #no<-randomForest(formula=fake ~ .,data_test=data_test,mtry=20,ntree=100)
  #no
  
  rf.fit
}


#y<-data_testnew[,5]
#x<-data_testnew[,-c(5)]
#bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)

#yes<-predict_random(data_testnew)
if(0){
datanew_test$fake<-seq(1,2950)
for(i in 1:nrow(datanew_test)) {
  a<-datanew_test[i,1:4]
  for(j in 1:nrow(datanew)){
    b<-datanew[j,1:4]
  if(b[,1]==a[,1]&b[,2]==a[,2]&b[,3]==a[,3]&b[,4]==a[,4]){
  datanew_test$fake[i]<-datanew[j,5]
  }
  }
}
}
