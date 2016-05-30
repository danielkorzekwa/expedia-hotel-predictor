library(e1071)

# clusterData [variables, hotel_cluster]
# clusterTestData [variables]
calcClusterProbsSVM <- function (clusterTrainData,clusterTestData) {
  
  clusterTrainData$hotel_cluster <- as.factor(clusterTrainData$hotel_cluster)
  
  model <- svm(hotel_cluster ~ ., data = clusterTrainData,probability=T,cross=2)
  print(summary(model))
  
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