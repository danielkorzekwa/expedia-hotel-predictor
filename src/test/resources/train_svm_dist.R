  rm(list=ls())

  train_13 <- fread('data_all/train_all_2013.csv')
  train_13 <- subset(train_13,is_booking > -1 & !is.na(orig_destination_distance))
  
  test_14 <- fread('data_booked/train_booked_2014_all_cols.csv')
  
 userLocMarketList <- sqldf('select user_location_city,hotel_market,count(*) from train_13 where is_booking > -1 group by user_location_city,hotel_market having count(*) > 100 order by count(*) desc')
 write.csv(userLocMarketList,'svm/svm_dist100/userLocMarketList.csv',row.names=F) 
 
  for(i in 1: nrow(userLocMarketList)) {
    userLoc <- userLocMarketList[i,1]
    market <- userLocMarketList[i,2]
    print(sprintf('training model %d for userloc/market= %d / %d',i,userLoc,market))
    
    train <- subset(train_13,is_booking > -1 & hotel_market==market & user_location_city==userLoc)
    
    svmData_a <- train[1:min(nrow(train),6000),c(7,24),with=F]
    svmData_a$hotel_cluster <- as.factor(svmData_a$hotel_cluster)
    model <- svm(hotel_cluster ~ ., data = svmData_a,probability=T,cross=2)
    summary(model)
    
    test <- subset(test_14, hotel_market==market & user_location_city==userLoc & !is.na(orig_destination_distance) )
    if(nrow(test)>0) {
    svmProbs <- predict(model,test[,c('orig_destination_distance'),with=F],probability=T)
    svmProbs <- attr(svmProbs, "probabilities")
    
    allClustersCols <- lapply(0:99,function(x) { x})
    missingClusterCols  <- setdiff(allClustersCols,colnames(svmProbs))
    missingClustersMat = matrix(0,nrow=nrow(svmProbs),ncol=length(missingClusterCols))
    colnames(missingClustersMat) <- missingClusterCols
    svmProbs <- cbind(svmProbs,missingClustersMat)
    svmProbs <- svmProbs[, order(as.integer(colnames(svmProbs)))]
    svmProbs <- data.frame(svmProbs)
    svmProbs$orig_destination_distance <- test$orig_destination_distance
    write.csv(svmProbs,sprintf('svm/svm_dist100/svm_predictions_loc_%d_market_%d.csv',userLoc,market),row.names=F)
    
    }
  }
  
  
  
  test <- cbind(test,svmProbs)
    ggplot() + geom_smooth(aes(col='50',x=test$orig_destination_distance,y=test$X50)) + 
    geom_smooth(aes(col='98',x=test$orig_destination_distance,y=test$X98)) + 
    coord_cartesian(xlim=c(-0.1,20),ylim=c(-0.1,1.1)) + scale_x_continuous(breaks=seq(0,20,1)) 

