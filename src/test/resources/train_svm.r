svmData_a <- subset(d1_a[1:100,c(24:173)],hotel_cluster > -1)
svmData_a$hotel_cluster <- as.factor(svmData_a$hotel_cluster)
svmData_a$srch_destination_id <- as.factor(svmData_a$srch_destination_id)

svmData_b <- subset(d1_b[1:10,24:173],hotel_cluster > -1)
svmData_b$hotel_cluster <- as.factor(svmData_b$hotel_cluster)

model <- svm(hotel_cluster ~ ., data = svmData_a,probability=F)
svmProbs <- predict(model,svmData_b,probability=F)
svmProbs <- attr(svmProbs, "probabilities")
svmProbs <- svmProbs[, order(as.integer(colnames(svmProbs)))]
write.csv(svmProbs,'svm_predictions.csv',row.names=F)

tuneResults <- tune.svm(hotel_cluster~., data = svmData_a, gamma = seq(.5, .9, by = .1), cost = seq(1,1000, by = 200))