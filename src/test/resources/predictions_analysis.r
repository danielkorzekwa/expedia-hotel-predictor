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

sqldf('select hotel_market,avg(mapk),count(*) from pp_3 group by hotel_market having count(*)>100 order by avg(mapk) limit 20')




#Analyse market user model
train_3 <- fread('c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.csv')
  test_3 <- fread('c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.csv')
p1 <- fread('c:/perforce/daniel/ex/predictions_analysis_2014/predictions_1.csv')
p5 <- fread('c:/perforce/daniel/ex/predictions_analysis_2014/predictions_5.csv')
test_3$mapk1 <- p1$mapk
test_3$mapk5 <- p5$mapk

test_3[hotel_continent==3 & hotel_market==12682 & srch_destination_id==8256][,c('date_time','user_location_country','user_id','srch_destination_id','hotel_market','hotel_cluster','orig_destination_distance','mapk1'),with=F][1:100][order(date_time)]