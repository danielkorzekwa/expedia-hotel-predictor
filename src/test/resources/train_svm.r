rm(list=ls())

train_13 <- fread('data_all/train_all_2013.csv')
train_13 <- subset(train_13,is_booking==1)

destIds <-  sqldf('select srch_destination_id,count(*) from train_13 group by srch_destination_id having count(*)>3000 order by count(*) desc')$srch_destination_id

for(dest_id in  destIds) {
  print(sprintf('training model=%d',dest_id))
  train <- subset(train_13,srch_destination_id==dest_id)
  train$time_to_ci <- as.Date(train$srch_ci) - as.Date(train$date_time)
  train$length <- as.Date(train$srch_co) - as.Date(train$srch_ci)
  
  svmData_a <- train[1:min(nrow(train),6000),c(24,26),with=F]
  svmData_a$hotel_cluster <- as.factor(svmData_a$hotel_cluster)
  model <- svm(hotel_cluster ~ ., data = svmData_a,probability=T,cross=2)
  summary(model)
  svmProbs <- predict(model,svmData_a,probability=T)
  svmProbs <- attr(svmProbs, "probabilities")
  
  allClustersCols <- lapply(0:99,function(x) { x})
  missingClusterCols  <- setdiff(allClustersCols,colnames(svmProbs))
  missingClustersMat = matrix(0,nrow=nrow(svmProbs),ncol=length(missingClusterCols))
  colnames(missingClustersMat) <- missingClusterCols
  svmProbs <- cbind(svmProbs,missingClustersMat)
  
  svmProbs <- svmProbs[, order(as.integer(colnames(svmProbs)))]
  
  svmProbs <- data.frame(svmProbs)
  svmProbs$length <- svmData_a$length
  write.csv(svmProbs,sprintf('svm/svm_predictions_%d.csv',dest_id),row.names=F)
}

#train <- subset(train,length<4)




tuneResults <- tune.svm(hotel_cluster~., data = svmData_a, gamma = seq(.5, .9, by = .1), cost = seq(1,1000, by = 200))