train_13_all <- fread('data_all/train_all_2013.csv')
train_13 <- subset(train_13_all,is_booking==1)

train_14 <- fread('data_booked/train_booked_2014_all_cols.csv')

ud1 <- subset(train_14,hotel_market==628 & user_location_city==24103 & hotel_cluster==1)
ud2 <- subset(train_14,hotel_market==628 & user_location_city==24103 & hotel_cluster==79)

ggplot() + 
geom_histogram(aes(col='1',x=ud1$orig_destination_distance),binwidth=0.1,alpha=0.5) +
geom_histogram(aes(col='79',x=ud2$orig_destination_distance),binwidth=0.1,alpha=0.5) + 
coord_cartesian(xlim=c(226,232))

s <- subset(train_13,hotel_market==628 & user_location_city==24103 & orig_destination_distance>227.5322-0.02 & orig_destination_distance < 227.5322+0.02)
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20')