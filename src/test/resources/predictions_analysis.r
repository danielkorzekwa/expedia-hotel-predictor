rm(list=ls())

#Predictions analysis
p <- read.csv('data_booked/predictions.csv')
p2 <- read.csv('data_booked/predictions2.csv')
train <- read.csv('data_booked/train_booked_2013.csv')
test <- read.csv('data_booked/train_booked_2014.csv')

#p <- read.csv('data_booked/predictions_sample_b.csv')
#train <- read.csv('data_booked/train_booked_sample_a.csv')
#test <- read.csv('data_booked/train_booked_sample_b.csv')

#analyze mapk
stat <- sqldf('select srch_destination_id,count(*) as c from train  group by srch_destination_id order by c')

#test <- merge(test,p,by='row.names')
test$p1 <- p$p1
test$p2 <- p$p2
test$r1 <- p$r1
test$r2 <- p$r2
test$mapk <- p$mapk

test$p1_2 <- p2$p1
test$p2_2 <- p2$p2
test$r1_2 <- p2$r1
test$r2_2 <- p2$r2
test$mapk2 <- p2$mapk



test <- merge(test,stat,all.x=T)
test$c[is.na(test$c)] <- 0
test  <- subset(test,p1 !=1 & p1_2 !=1)

ggplot() + geom_smooth(aes(col='mapk',x=test$c,y=test$mapk)) +
  geom_smooth(aes(col='mapk2',x=test$c,y=test$mapk2)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(0,400))

ggplot() + stat_summary(fun.y=mean,geom='point',aes(col='mapk',x=test$c,y=test$mapk)) +
   stat_summary(fun.y=mean,geom='point',aes(col='mapk2',x=test$c,y=test$mapk2)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-1,200))

