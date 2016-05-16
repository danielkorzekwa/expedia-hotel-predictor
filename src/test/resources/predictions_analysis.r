rm(list=ls())

#Predictions analysis)
pb <- fread('predictions_analysis_2014/predictions_best.csv')
pb <- pb[,-c('hotel_cluster'),with=F]

p2 <- fread('predictions_analysis_2014/predictions2.csv')
p1 <- fread('predictions_analysis_2014/predictions.csv')
colnames(p1) <- c('p1_1','p2_1','p3_1','p4_1','p5_1','r1_1','r2_1','r3_1','r4_1','r5_1','hotel_cluster_1','mapk_1')
colnames(p2) <- c('p1_2','p2_2','p3_2','p4_2','p5_2','r1_2','r2_2','r3_2','r4_2','r5_2','hotel_cluster_2','mapk_2')

c1_all <- fread('predictions_analysis_2014/calibration.csv')
c2_all <- fread('predictions_analysis_2014/calibration2.csv')
c3_all <- fread('predictions_analysis_2014/calibration3.csv')

c1 <- subset(c1_all,p> -1 & p< 1.1)
c2 <- subset(c2_all,p> 0.0 & p < 0.02)
c3 <- subset(c3_all,p> -1 & p< 1.1)

ggplot() + 
  #geom_smooth(aes(col='c1',x=c1$p,c1$actual)) + 
  geom_smooth(aes(col='c2',x=c1$p,c1$actual)) + 
  scale_x_continuous(breaks=seq(0,1,0.05)) + 
  scale_y_continuous(breaks=seq(0,1,0.05)) 



test <- fread('data_booked/train_booked_2014_all_cols.csv')

pp <- cbind(test,p1,p2)

ggplot() + geom_smooth(aes(col='p1',x=p1$p1,y=p1$mapk)) + geom_smooth(aes(col='p2',x=p2$p1,y=p2$mapk)) + 
  scale_x_continuous(breaks=seq(0,1,0.05)) +
  xlim(0.0,1)

test <- fread('data_booked/train_booked_2014_all_cols.csv')             
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

