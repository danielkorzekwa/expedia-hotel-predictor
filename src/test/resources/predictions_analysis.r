rm(list=ls())

#Predictions analysis
p <- read.csv('data_booked/predictions_sample_b.csv')
p_svm <- read.csv('data_booked/predictions_sample_b_svm.csv')
train <- read.csv('data_booked/train_booked_sample_a.csv')
test <- read.csv('data_booked/train_booked_sample_b.csv')

#analyze mapk
stat <- sqldf('select srch_destination_id,count(*) as c from train  group by srch_destination_id order by c')

test <- merge(test,stat,all.x=T)
test$c[is.na(test$c)] <- 0
test <- merge(test,p,by='row.names')
test$mapk_svm <- p_svm$mapk
test$p1_svm <- p_svm$p1

ggplot() + geom_smooth(aes(col='mapk',x=b$c,y=b$mapk)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(0,400))

ggplot() + stat_summary(fun.y=mean,geom='point',aes(col='mapk',x=test$c,y=test$mapk)) +
   stat_summary(fun.y=mean,geom='point',aes(col='mapk_svm',x=test$c,y=test$mapk_svm)) +
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-1,100))
