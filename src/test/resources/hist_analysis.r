train_13_all <- fread('data_all/train_all_2013.csv')

train_13 <- subset(train_13_all,is_booking > -1)
train_13$month <- month(as.Date(train_13$srch_ci))
train_13$weekday <- weekdays(as.Date(train_13$srch_ci))

train_14 <- fread('data_booked/train_booked_2014_all_cols.csv')


ud1 <- subset(train_13,hotel_market==365 & user_location_city==24103 & hotel_cluster==19)
ud2 <- subset(train_13,hotel_market==365 & user_location_city==24103 & hotel_cluster==50)

ggplot() + 
geom_histogram(aes(col='1',x=ud1$orig_destination_distance),binwidth=0.1,alpha=0.5) +
geom_histogram(aes(col='2',x=ud2$orig_destination_distance),binwidth=0.1,alpha=0.5) + 
scale_x_continuous(breaks=seq(0,20,1)) +
coord_cartesian(xlim=c(0,20))

head(subset(pp,user_location_city==24103 & hotel_market==365 & p1<0.2 & orig_destination_distance==15.0772))


s <- subset(train_13,hotel_market==628 & user_location_city==24103 & orig_destination_distance>227.5322-0.02 & orig_destination_distance < 227.5322+0.02)
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 20')

#single cluster plots
ud1 <- subset(train_13,hotel_market==365 & hotel_cluster==98 & !is.na(orig_destination_distance))
ud2 <- subset(train_13,hotel_market==365 & hotel_cluster==97 & !is.na(orig_destination_distance))
ggplot() + 
  geom_histogram(aes(col='1',x=ud1$user_location_city),alpha=0.5) +
  geom_histogram(aes(col='2',x=ud2$user_location_city),alpha=0.5)





ud1 <- subset(train_13,hotel_market==628 & user_location_city==24103 & hotel_cluster==1)
ud2 <- subset(train_13,hotel_market==628 & user_location_city==24103 & hotel_cluster==79)

ggplot() + 
  geom_histogram(aes(col='1',x=ud1$orig_destination_distance),binwidth=0.1,alpha=0.5) +
  geom_histogram(aes(col='79',x=ud2$orig_destination_distance),binwidth=0.1,alpha=0.5) + 
  coord_cartesian(xlim=c(220,240))