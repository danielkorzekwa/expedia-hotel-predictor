train_13 <- fread('data_all/train_all_2013.csv')

s <- subset(pp, r1==91)
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20' )





d <- sqldf('select srch_destination_id,avg(mapk1) mapk1,avg(mapk1000) mapk1000,count(*) c from test group by srch_destination_id')
subset(d,c>100 & mapk1000-mapk1   > 0.1)


s61 <- subset(train_13,    hotel_cluster== 4 & hotel_continent==2 & is_booking==1 )
s81 <- subset(train_13,   hotel_cluster==6 & hotel_continent==2 & is_booking==1)

ggplot() + geom_freqpoly(aes(col='s61',x=as.Date(s61$srch_ci)),binwidth = 7,alpha=0.5) +
  geom_freqpoly(aes(col='s81',x=as.Date(s81$srch_ci)),binwidth = 7,alpha=0.5) 

s <- subset(pp,r1==4 & hotel_cluster==6 & p1==1 )
ggplot() + geom_freqpoly(aes(col='s61',x=as.Date(s$srch_ci)),binwidth = 7,alpha=0.5) 