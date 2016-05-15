train_13_all <- fread('data_all/train_all_2013.csv')
train_13 <- subset(train_13_all,is_booking > -1)

ud1 <- subset(train_13,hotel_market==365 & user_location_city==24103 & hotel_cluster==50)
ud2 <- subset(train_13,hotel_market==365 & user_location_city==24103 & hotel_cluster== 91)

ggplot() + 
geom_histogram(aes(col='1',x=ud1$orig_destination_distance),binwidth=0.1,alpha=0.5) +
geom_histogram(aes(col='2',x=ud2$orig_destination_distance),binwidth=0.1,alpha=0.5) + 
scale_x_continuous(breaks=seq(0,20,1)) +
coord_cartesian(xlim=c(0,20))

s <- subset(train_13,hotel_market==1532 & user_location_city==26232 )
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20')

