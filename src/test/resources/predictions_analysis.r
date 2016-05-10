rm(list=ls())

#Predictions analysis
p <- fread('predictions_analysis_2014/predictions.csv')
p2 <- fread('predictions_analysis_2014/predictions2.csv')

test <- fread('data_booked/train_booked_2014_all_cols.csv')

pp <- cbind(test,p2)
pp$mapk1 <- p$mapk
subset(pp,hotel_market==628 & user_location_city==24103 & orig_destination_distance>227.2 & orig_destination_distance < 227.8 & p1<1)

#analyze mapk
stat <- sqldf('select srch_destination_id,count(*) as c from train  group by srch_destination_id order by c')

testc <- test
testc$p1 <- p$p1
testc$p2 <- p$p2
testc$r1 <- p$r1
testc$r2 <- p$r2
testc$mapk <- p$mapk

testc$p1_2 <- p2$p1
testc$p2_2 <- p2$p2
testc$r1_2 <- p2$r1

testc$r2_2 <- p2$r2
testc$mapk2 <- p2$mapk

testc <- merge(testc,stat,all.x=T,sort=FALSE)
testc$c[is.na(testc$c)] <- 0
testc  <- subset(testc,p1 !=1 )

ggplot() + geom_smooth(aes(col='mapk',x=test$c,y=test$mapk)) +
  geom_smooth(aes(col='mapk2',x=test$c,y=test$mapk2)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(0,400))

ggplot() + stat_summary(fun.y=mean,geom='point',aes(col='mapk',x=testc$c,y=testc$mapk)) +
 #  stat_summary(fun.y=mean,geom='point',aes(col='mapk2',x=test$c,y=test$mapk2)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-1,100))

