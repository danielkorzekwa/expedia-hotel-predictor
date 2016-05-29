rm(list=ls())

#Predictions analysis)
pb <- fread('c:/perforce/daniel/ex/predictions_analysis_2014/predictions_best.csv')
test_all <- fread('c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv')
pb <- pb[,-c('hotel_cluster'),with=F]
pp <- cbind(test_all,pb)




train_13[hotel_market==675][,.N,by=srch_destination_id][order(-N)][srch_destination_id==7617]
train_13[hotel_country==50][,.N,by=hotel_cluster][order(-N)][1:10]