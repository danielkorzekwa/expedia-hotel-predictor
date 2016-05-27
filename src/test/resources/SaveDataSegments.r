train <- fread('c:/perforce/daniel/ex/orig_data/train.csv')
train_b <- subset(train,is_booking==1)

train_13 <- subset(train,as.Date(date_time) < as.Date('2014-01-01'))
train_14b <- subset(train_b,as.Date(date_time) >= as.Date('2014-01-01'))




write.csv(subset(train_from140101_until140701),'c:/perforce/daniel/ex/segments/by6months/train_140101_140701.csv',row.names=F,quote=F)
write.csv(subset(train_14b_from0701),'c:/perforce/daniel/ex/segments/by6months/train_140701_150101_booked_only.csv',row.names=F,quote=F)