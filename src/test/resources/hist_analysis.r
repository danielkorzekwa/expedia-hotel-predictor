train_13_all <- fread('data_all/train_all_2013.csv')
train_13 <- subset(train_13_all,is_booking > -1)

ud1 <- subset(train_13, user_location_region==346 & hotel_market==628 & hotel_cluster==54)
ud2 <- subset(train_13, user_location_region==346 & hotel_market==628 &  hotel_cluster==91)

ggplot() + 
geom_histogram(aes(col='54',x=ud1$orig_destination_distance),binwidth=0.1,alpha=0.5) +
geom_histogram(aes(col='91',x=ud2$orig_destination_distance),binwidth=0.1,alpha=0.5) + 
scale_x_continuous(breaks=seq(0,50,5)) +
coord_cartesian(xlim=c(0,50))

s <- subset(pp , user_location_region==174 & srch_destination_id==8250 & hotel_market==628 )
sqldf('select hotel_cluster,count(*) as c from s group by hotel_cluster order by c desc limit 10')



# correlated clusters in test by cluster_dist, choose the higest prediction across all clicks for that dist in test
head(subset(pp, p1<1 & user_location_city==24103 & hotel_market==628 & srch_destination_id==8250 & hotel_cluster==49),50)
head(subset(pp, p1<1 & user_location_city==24103 & hotel_market==628 & srch_destination_id==8250 & mapk>0),50)
subset(pp, user_id==1143331 & hotel_market==628)