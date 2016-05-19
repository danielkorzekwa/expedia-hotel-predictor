rm(list=ls())

#Predictions analysis)
pb <- fread('predictions_analysis_2014/predictions_best.csv')
test_all <- fread('data_booked/train_booked_2014_all_cols.csv')
pb <- pb[,-c('hotel_cluster'),with=F]
pp <- cbind(test_all,pb)
