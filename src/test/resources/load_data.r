
#
# Read data
#

d1 <- read.csv('data_booked/train_booked_all.csv',nrow=50000000)

d1_a <- subset(d1,as.Date(date_time)<as.Date('2014-01-01'))[1:20000,]
d1_b <- subset(d1,as.Date(date_time)>=as.Date('2014-01-01'))[1:20000,]
d1_ab <- d1

d1_a$orig_destination_distance[is.na(d1_a$orig_destination_distance)] <- -1
d1_b$orig_destination_distance[is.na(d1_b$orig_destination_distance)] <- -1
d1_ab$orig_destination_distance[is.na(d1_ab$orig_destination_distance)] <- -1

#d1_a <- subset(d1_a,srch_destination_id==8250)
#d1_b <- subset(d1_b,srch_destination_id==8250)

dest <- read.csv('destinations.csv')
d1_a <- merge(d1_a,dest)[, union(names(d1_a), names(dest))]
d1_b <- merge(d1_b,dest)[, union(names(d1_b), names(dest))]

rownames(d1_a) <- seq(length=nrow(d1_a))
rownames(d1_b) <- seq(length=nrow(d1_b))

category <- c('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market','hotel_cluster')
write.csv(d1_a[category],'train_booked_2013.csv',row.names=FALSE)
write.csv(d1_b[category],'train_booked_2014.csv',row.names=FALSE)

write.csv(d1_ab[category],'train_booked_all_ab.csv',row.names=FALSE)


# Test data


t<- read.csv('orig_data/test.csv')

t$orig_destination_distance[is.na(t$orig_destination_distance)] <- -1

testCat <- c('user_location_city','orig_destination_distance','user_id','srch_destination_id','hotel_market')
write.csv(t[testCat],'test_all.csv',row.names=FALSE)
