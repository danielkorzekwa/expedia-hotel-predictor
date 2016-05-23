train <- fread('c:/perforce/daniel/ex/orig_data/train.csv')
train_b <- subset(train,is_booking==1)

train_13 <- subset(train,as.Date(date_time) < as.Date('2014-01-01'))
train_14b <- subset(train_b,as.Date(date_time) >= as.Date('2014-01-01'))
write.csv(subset(train_13,hotel_continent==3),'c:/perforce/daniel/ex/segments/continent_3/train_2013_continent3.csv',row.names=F,quote=F)
write.csv(subset(train_14b,hotel_continent==3),'c:/perforce/daniel/ex/segments/continent_3/train_2014_continent3_booked_only.csv',row.names=F,quote=F)