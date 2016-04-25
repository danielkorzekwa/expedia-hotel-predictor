rm(list=ls())

#Predictions analysis
p <- read.csv('data_booked/predictions_sample_b.csv')
train <- read.csv('data_booked/train_booked_sample_a.csv')
test <- read.csv('data_booked/train_booked_sample_b.csv')

#analyze mapk
stat <- sqldf('select srch_destination_id,count(*) as c from train  group by srch_destination_id order by c')

test <- merge(test,stat,all.x=T)
test$c[is.na(test$c)] <- 0
test <- merge(test,p,by='row.names')
test  <- subset(test,p1 !=1)

ggplot() + geom_smooth(aes(col='mapk',x=b$c,y=b$mapk)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(0,400))

ggplot() + stat_summary(fun.y=mean,geom='point',aes(col='mapk',x=test$c,y=test$mapk)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-1,100))

