library(lattice)
library(caret)
require(randomForest)

setwd("D:\\others\\AnomalyDetection-master\\challenges\\2\\challenge_data.tar\\challenge_data")
data<-read.csv("train_fakenes.csv", sep=",",header = FALSE,dec = ".")
#data<-data[,-c(4,11)]
data<-data[,-c(4)]
#data<-data[complete.cases(data),]
data1<-read.delim("yelp_data_reviewer.dat",header = TRUE,sep=";",dec=".",stringsAsFactors = FALSE)
data2<-read.delim("yelp_data_hotel.dat",header = TRUE,sep=";",dec=".",stringsAsFactors = FALSE)
names(data)<-c("date","reviewID","reviewerID","rating","usefulCount","coolCount","funnyCount","fake","hotelID","Fakeness","Rev_len")


temp_data<-data.frame(0,0,0,0,0,0,0,0,0,0,0,0)
temp1_data<-data.frame(1,2,3,4,5,6,7,8,9,10,11,12,13)
rows_invalid<-c()
rows_invalid_new<-c()
names(temp_data)<-c("reviewerid","location_rev","joindate","friend_count_rev","reviewcount","firstcount","usefulcount","coolcount","funnycount","complimentcount","tipcount","fancount")
names(temp1_data)<-c("hotelID","name", "location_hotel","reviewCount","rating","categories","address","AcceptsCreditCards","PriceRange","WiFi", "webSite", "phoneNumber", "filReviewCount")
for(i in 1:nrow(data))
{
  temp<-data$reviewerID[i]
  temp1<-data$hotelID[i]
  # avira<-data1$reviewerID==temp
  #new<-avira==TRUE
  avi<-data1[data1$reviewerID==temp,c(1,3,4,5:13)]
  avi1<-data2[data2$hotelID==temp1,]
  if(nrow(avi)==0)
  {
    rows_invalid<-rbind(rows_invalid,i)
  }
  if(nrow(avi1)==0)
  {
    rows_invalid_new<-rbind(rows_invalid_new,i)
  }
  avi<-as.data.frame(avi,row.names = NULL)
  avi1<-as.data.frame(avi1,row.names = NULL)
  names(avi)<-c("reviewerid","location_rev","joindate","friend_count_rev","reviewcount","firstcount","usefulcount","coolcount","funnycount","complimentcount","tipcount","fancount")
  names(avi1)<-c("hotelID","name", "location_hotel","reviewCount","rating","categories","address","AcceptsCreditCards","PriceRange","WiFi", "webSite", "phoneNumber", "filReviewCount")
  temp_data<-rbind(temp_data,avi)
  temp1_data<-rbind(temp1_data,avi1)
  
  
}

temp_data<-temp_data[2:nrow(temp_data),]
temp1_data<-temp1_data[2:nrow(temp1_data),]
temp1_data<-temp1_data[-rows_invalid,]
rows_invalid_new<-rbind(rows_invalid_new,rows_invalid)
data<-data[-rows_invalid_new,]
data<-cbind(data,temp_data,temp1_data)
data[,c(12,24)]<-NULL


temp3<-c(1,2,3,8,9,12,13,23,24,27:34)
for(i in 1:length(temp3))
{
  data[,temp3[i]]<-as.factor(data[,temp3[i]])
}
datanew<-data[,-c(1,2,3,9,12,13,23,24,28,32,33)]
#datanew<-datanew[,-c(6)]

datanew[,8]<-as.numeric(datanew[,8])
#datanew[,21]<-as.numeric(datanew[,21])
#dataavi<-datanew[,-c(17,18,19,20,21)]
predict_random<-function(data){
  #avi<-seq(60,70,2)
  #rf.fit<-list(5,1,2,3,4,5)
 # for(i in 1:6)
  #{
  
  tr_control<-trainControl(method="cv", number=5, repeats=1,allowParallel = T)
 # grid <- expand.grid(C = seq(0,1,0.01))
  grid <- expand.grid(mtry =seq(4,15,3))
  rf.fit<- train(fake ~ .,data=data,method="rf",trControl=tr_control,tuneGrid=grid,ntrees=68)
  #rf.fit <- train(fake ~ .,data=data,method="rf",ntrees=avi[i],mtry=20)
  #rf.fit <- train(fake ~ .,data=data,method="svmLinear",trControl=tr_control)
  #rf.fit<-randomForest(fake ~ .,data=data,ntree=68,mtry=4)
 # }
 rf.fit
}



if(0)
{
C<-c(0.01,1,10,15,40,75,100,1000)
svp<-list(1,2,3,4,5,6,7,8)
for (i in 1:8)
{
  svp[[i]] <- ksvm(fake ~ .,data=dataavi,type="C-svc",kernel='vanilladot',C=C[i],cross=5)
}
}


#rf<-predict_random(datanew)

#y<-datanew[,5]
#x<-datanew[,-c(5)]
#bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)


#output<-predict(rf,dataavi)

if(0){
datanew_test$fake<-seq(1,2950)

for(i in 1:nrow(datanew_test)) {
  
  nzeros<-c(0)
  nones<-c(0)
  a<-datanew_test[i,1:4]
  for(j in 1:nrow(datanew))
    {
        b<-datanew[j,1:4]
          if(a[,1]==b[,1]&a[,2]==b[,2]&a[,3]==b[,3]&a[,4]==b[,4])
        {
           if(datanew[j,5]==0)
               {
                nzeros<-nzeros+1
                }
          if(datanew[j,5]==1)
             {
               nones<-nones+1
              }
      
    #
    }
    if(nzeros|nones)
      {
          if(nzeros>nones)
            {
              datanew_test$fake[i]<-c(0)
            }
          else
           datanew_test$fake[i]<-c(1)
      }
    
  }
  
}
}
  #mini<-which(c==min(c),arr.ind = TRUE)
  #temp<-mini[,1]
  #if(temp){
  #datanew_test$fake[i]<-datanew[temp,5]
  #}
#}