rm(list=ls())

#
#Analysis
#
cluster <-12
ggplot() + geom_histogram(aes(col='all',x=subset(d1_a,hotel_cluster!=cluster)$srch_children_cnt))  +
  ggplot() +  geom_histogram(aes(col='single',x=subset(b,hotel_cluster==cluster)$srch_children_cnt)) + coord_cartesian(xlim=c(-10,10))

qplot(subset(d1_ad,hotel_cluster!=90)$d1) + coord_cartesian(xlim=c(-2.50,-1.5))
ggplot() +stat_summary(fun.y=mean,geom='point',aes(x=d$orig_destination_distance,y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))
ggplot() +stat_summary(fun.y=mean,geom='line',aes(x=as.Date(d$date_time),y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))


ggplot() +geom_smooth(aes(x=as.Date(d$srch_ci),y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))

sqldf('select srch_destination_id,count(*) as c from d1_a group by srch_destination_id order by c')
sqldf('select hotel_cluster,count(*) as c from d1_a where srch_destination_id=11439 group by hotel_cluster order by c')


#Read predictions
p3 <- read.csv('predictions.csv')

#analyze mapk
a <- sqldf('select srch_destination_id,count(*) as c from d1_a  group by srch_destination_id order by c')

b <- merge(d1_b,a,all.x=T)
b$c[is.na(b$c)] <- 0
b$mapk <- p$mapk
b$mapk2 <- p2$mapk
b$mapk3 <- p3$mapk

ggplot() + geom_smooth(aes(col='mapk',x=b$c,y=b$mapk)) + 
  geom_smooth(aes(col='mapk2',x=b$c,y=b$mapk2)) + 
  geom_smooth(aes(col='mapk3',x=b$c,y=b$mapk3)) + 
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(0,400))

ggplot() + stat_summary(fun.y=mean,geom='point',aes(col='mapk',x=b$c,y=b$mapk)) + 
  stat_summary(fun.y=mean,geom='point',aes(col='mapk2',x=b$c,y=b$mapk2)) + 
  stat_summary(fun.y=mean,geom='point',aes(col='mapk3',x=b$c,y=b$mapk3)) + 
  coord_cartesian(ylim=c(-0.1,1.1),xlim=c(-1,200))

