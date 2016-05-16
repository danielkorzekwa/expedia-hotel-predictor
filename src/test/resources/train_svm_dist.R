library(data.table)
library(e1071)
library(ggplot2)
library(sqldf)


# clusterData [distance, hotel_cluster]
# clusterTestData [distance]
calcClusterProbsSVM <- function (clusterTrainData,clusterTestData) {
  
  clusterTrainData$hotel_cluster <- as.factor(clusterTrainData$hotel_cluster)
  
  model <- svm(hotel_cluster ~ ., data = clusterTrainData,probability=T,cross=2)
  summary(model)
  
  svmProbs <- predict(model,clusterTestData,probability=T)
  svmProbs <- attr(svmProbs, "probabilities")
    
  allClustersCols <- lapply(0:99,function(x) { x})
  missingClusterCols  <- setdiff(allClustersCols,colnames(svmProbs))
  missingClustersMat = matrix(0,nrow=nrow(svmProbs),ncol=length(missingClusterCols))
  colnames(missingClustersMat) <- missingClusterCols
  svmProbs <- cbind(svmProbs,missingClustersMat)
  svmProbs <- svmProbs[, order(as.integer(colnames(svmProbs)))]
  svmProbs <- data.frame(svmProbs)
 
  return(svmProbs)
  
}


 

 rm(list=ls())

  train_13 <- fread('data_all/train_all_2013.csv')
  train_13 <- subset(train_13,is_booking > -1 & !is.na(orig_destination_distance))
  
  test_14 <- fread('data_booked/train_booked_2014_all_cols.csv')
  
  userLocMarketList <- sqldf('select user_location_city,hotel_market,srch_destination_id, count(*) from train_13 where is_booking > -1 group by user_location_city,hotel_market,srch_destination_id having count(*) > 1000 order by count(*) desc')
 write.csv(userLocMarketList,'svm/svm_dest_dist1000/userLocMarketList.csv',row.names=F) 
 
  for(i in 215: nrow(userLocMarketList)) {
    
    tryCatch(
      {
       
        userLoc <- userLocMarketList[i,1]
        market <- userLocMarketList[i,2]
        dest <- userLocMarketList[i,3]
        print(sprintf('training model %d for userloc/market/dest= %d/%d/%d',i,userLoc,market,dest))
        
        train <- subset(train_13,is_booking > -1 & hotel_market==market & user_location_city==userLoc & srch_destination_id==dest)
        train <- train[1:min(nrow(train),6000),c(7,24),with=F]
    
        test <- subset(test_14, hotel_market==market & user_location_city==userLoc & srch_destination_id==dest & !is.na(orig_destination_distance) )
        test <- test[,c('orig_destination_distance'),with=F]
    
        if(nrow(test)>0) {
          svmProbs <- calcClusterProbsSVM(train,test)
          svmProbs$orig_destination_distance <- test$orig_destination_distance
          write.csv(svmProbs,sprintf('svm/svm_dest_dist1000/svm_predictions_loc_%d_market_%d_dest_%d.csv',userLoc,market,dest),row.names=F)
        }
         
      },
      error = function(err) {}
      )
    
   
  }
  
  
 svmProbs <- calcClusterProbsSVM(t,t)
  test <- cbind(t,svmProbs)
    ggplot() + geom_line(aes(col='54',x=test$orig_destination_distance,y=test$X54)) + 
      geom_line(aes(col='1',x=test$orig_destination_distance,y=test$X1)) + 
    coord_cartesian(xlim=c(44,47),ylim=c(-0.1,1.1)) + scale_x_continuous(breaks=seq(44,47,0.5)) 

