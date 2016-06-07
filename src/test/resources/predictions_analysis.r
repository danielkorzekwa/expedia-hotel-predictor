rm(list=ls())

#Predictions analysis)

train_13 <- fread('c:/perforce/daniel/ex/segments/all/train_2013.csv')
pb <- fread('c:/perforce/daniel/ex/predictions_analysis_2014/predictions_best.csv')
test_all <- fread('c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv')
pb <- pb[,-c('hotel_cluster'),with=F]
pp <- cbind(test_all,pb)

clusters <- fread('c:/perforce/daniel/ex/statistics/clusterByDest_30k.csv')
pp <- merge(pp,clusters,by=c('srch_destination_id'),sort=F)
train_13 <- merge(train_13,clusters,by=c('srch_destination_id'),sort=F)

a <- sqldf('select hotel_cluster,avg(mapk),count(*) from pp group by hotel_cluster order by hotel_cluster limit 100')

dest <- fread('c:/perforce/daniel/ex/orig_data/destinations.csv')
