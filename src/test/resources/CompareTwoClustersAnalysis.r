rm(list=ls())

train_13 <-  fread('c:/perforce/daniel/ex/segments/all/train_2013.csv')


s <- subset(train_13,  user_location_city==2096 & hotel_market==675 & srch_destination_id==8267 )
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20' )


s1 <- subset(s,    hotel_cluster==   56  )
s2 <- subset(s,   hotel_cluster==   41 & orig_destination_distance>3000 )

ggplot() + geom_histogram(aes(col='s1',x=s1$orig_destination_distance),binwidth = 0.1,alpha=0.5) +
  geom_histogram(aes(col='s2',x=s2$orig_destination_distance),binwidth = 0.1,alpha=0.5) +
  coord_cartesian(xlim=c(3900,4100))

