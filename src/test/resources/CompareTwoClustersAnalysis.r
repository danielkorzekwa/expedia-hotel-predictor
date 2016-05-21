rm(list=ls())
train <- fread('c:/perforce/daniel/ex/data_all/train_all_2013.csv')
test <- fread('c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv')

s <- subset(train_13,  srch_destination_id==12217 & is_booking==1)
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20' )


s61 <- subset(train,    hotel_cluster==   19 & srch_destination_id==12217 & is_booking == 1 )
s81 <- subset(train,   hotel_cluster==   23 &  srch_destination_id==12217 & is_booking ==1)

ggplot() + geom_freqpoly(aes(col='s61',x=as.Date(s61$date_time)),binwidth = 30,alpha=0.5) +
  geom_freqpoly(aes(col='s81',x=as.Date(s81$date_time)),binwidth = 30,alpha=0.5) 

s <- subset(pp,r1==4 & hotel_cluster==6 & p1==1 & hotel_continent==2 & hotel_market==365 )
ggplot() + geom_freqpoly(aes(col='s61',x=as.Date(s$srch_ci)),binwidth = 7,alpha=0.5) 
