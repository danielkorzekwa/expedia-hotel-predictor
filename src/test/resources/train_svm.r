rm(list=ls())

train <- fread('data_booked/train_booked_2014_all_cols.csv')
train <- subset(train,srch_destination_id==8250 & hotel_market == 628)
train$time_to_ci <- as.Date(train$srch_ci) - as.Date(train$date_time)
train$length <- as.Date(train$srch_co) - as.Date(train$srch_ci)
train <- subset(train,length<4)

svmData_a <- train[1:6000,c(24,26),with=F]
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

svmProbs <- svmProbs[, order(colnames(svmProbs))]

svmProbs <- data.frame(svmProbs)
svmProbs$length <- svmData_a$length
write.csv(svmProbs,'svm/svm_predictions_8250_628_by_staylength.csv',row.names=F)


tuneResults <- tune.svm(hotel_cluster~., data = svmData_a, gamma = seq(.5, .9, by = .1), cost = seq(1,1000, by = 200))