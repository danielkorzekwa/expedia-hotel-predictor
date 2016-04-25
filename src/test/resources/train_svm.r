rm(list=ls())
train <- read.csv('data_booked/train_booked_sample_a.csv')
test <- read.csv('data_booked/train_booked_sample_b.csv')
dest <- read.csv('orig_data/destinations.csv')

train <- merge(train,dest)[, union(names(train), names(dest))]
test <- merge(test,dest)[, union(names(test), names(dest))]
rownames(train) <- seq(length=nrow(train))
rownames(test) <- seq(length=nrow(test))

svmData_a <- train[,6:155]
svmData_a$hotel_cluster <- as.factor(svmData_a$hotel_cluster)

svmData_b <- test[,6:155]
svmData_b$hotel_cluster <- as.factor(svmData_b$hotel_cluster)

model <- svm(hotel_cluster ~ ., data = svmData_a,probability=T)
svmProbs <- predict(model,svmData_b,probability=T)
svmProbs <- attr(svmProbs, "probabilities")
svmProbs <- svmProbs[, order(as.integer(colnames(svmProbs)))]
write.csv(svmProbs,'data_booked/svm_predictions_sample_b.csv',row.names=F)

tuneResults <- tune.svm(hotel_cluster~., data = svmData_a, gamma = seq(.5, .9, by = .1), cost = seq(1,1000, by = 200))