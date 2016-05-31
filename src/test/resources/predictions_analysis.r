rm(list=ls())

#Predictions analysis)
pb <- fread('c:/perforce/daniel/ex/predictions_analysis_2014/predictions_best.csv')
test_all <- fread('c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv')
pb <- pb[,-c('hotel_cluster'),with=F]
pp <- cbind(test_all,pb)


sqldf('select hotel_market,avg(mapk),count(*) from pp_3 group by hotel_market having count(*)>100 order by avg(mapk) limit 20')
