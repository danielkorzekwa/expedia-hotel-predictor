library(data.table)
library(ggplot2)
library(sqldf)

source('calcClusterProbsSVM.r')

 rm(list=ls())

  train_13 <-  fread('c:/perforce/daniel/ex/segments/all/train_2013.csv')
  train_13 <- subset(train_13,is_booking > -1 & !is.na(orig_destination_distance))
  
  test_14 <- fread('c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv')
  test_14 <- test_14[,-c('hotel_cluster'),with=F]
  test_15 <- fread('c:/perforce/daniel/ex/data_test/test_all_all_cols.csv')
  test_14_15 <- rbind(test_14,test_15)
  
  userLocMarketList <- sqldf('select user_location_city,hotel_market,srch_destination_id, count(*) c from train_13 where is_booking > -1 group by user_location_city,hotel_market,srch_destination_id having count(*) > 100 order by count(*) desc')
 write.csv(userLocMarketList,'c:/perforce/daniel/ex/svm/svm_dest_dist100/userLocMarketList.csv',row.names=F) 
 
  for(i in 1: nrow(userLocMarketList)) {
    
    tryCatch(
      {
       
        userLoc <- userLocMarketList[i,1]
        market <- userLocMarketList[i,2]
        dest <- userLocMarketList[i,3]
        print(sprintf('training model %d for userloc/market/dest= %d/%d/%d',i,userLoc,market,dest))
        
        train <- subset(train_13,is_booking > -1 & hotel_market==market & user_location_city==userLoc & srch_destination_id==dest)
        train <- train[1:min(nrow(train),10000),c(7,24),with=F]
    
        #test
        test <- subset(test_14_15, hotel_market==market & user_location_city==userLoc & srch_destination_id==dest & !is.na(orig_destination_distance) )
        test <- test[,c('orig_destination_distance'),with=F]
    
        if(nrow(test)>0) {
          svmProbs <- calcClusterProbsSVM(train,test)
          svmProbs$orig_destination_distance <- test$orig_destination_distance
          write.csv(svmProbs,sprintf('c:/perforce/daniel/ex/svm/svm_dest_dist100/svm_predictions_loc_%d_market_%d_dest_%d.csv',userLoc,market,dest),row.names=F)
        }
      
      },
      error = function(err) {}
      )
    
   
  }
  
 #Analysis
 
 train <-  subset(train_13,  user_location_city==2096 & hotel_market==675 & srch_destination_id==8267 ) 
 train[1:min(nrow(train),10000),c(7,24),with=F]
 
 test <- train
 test[,c('orig_destination_distance'),with=F]
 
 svmProbs <- calcClusterProbsSVM(train,test)
 
 
  
    ggplot() + geom_line(aes(col='1',x=test$orig_destination_distance,y=svmProbs$X56)) + 
      geom_line(aes(col='2',x=test$orig_destination_distance,y=svmProbs$X41)) + 
    coord_cartesian(xlim=c(4016,4018),ylim=c(-0.1,1.1)) 

