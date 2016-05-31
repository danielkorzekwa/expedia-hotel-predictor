library(sqldf)
library(data.table)

rm(list=ls())

train_13 <- fread('c:/perforce/daniel/ex/segments/all/train_2013.csv')
train_13 <- subset(train_13,!is.na(orig_destination_distance))

test_all <- fread('c:/perforce/daniel/ex/segments/all/train_2014_booked_only.csv')

userLocMarketList <- sqldf('select user_location_city,hotel_market,count(*) c from train_13 group by user_location_city,hotel_market having count(*)>100 order by count(*) desc')

write.csv(userLocMarketList,'c:/perforce/daniel/ex/segments/loc_market/more_than_100/userLocMarketList.csv',row.names=F)

#Save training set
for(i in 1:nrow(userLocMarketList)) {
  userLoc <- userLocMarketList[i,1]
  marketId <- userLocMarketList[i,2]
  print(sprintf('Saving training data %d',i))
  s <- train_13[user_location_city==userLoc & hotel_market==marketId]
  write.csv(s,sprintf('c:/perforce/daniel/ex/segments/loc_market/more_than_100/train_2013_loc_%d_market_%d.csv',userLoc,marketId),row.names=F,quote=F)
}

#Save test set
for(i in 1:nrow(userLocMarketList)) {
  userLoc <- userLocMarketList[i,1]
  marketId <- userLocMarketList[i,2]
  destId <- userLocMarketList[i,3]
  print(sprintf('Saving test data %d',i))
  s <- test_all[user_location_city==userLoc & hotel_market==marketId]
  write.csv(s,sprintf('c:/perforce/daniel/ex/segments/loc_market/more_than_100/train_2014_booked_only_loc_%d_market_%d.csv',userLoc,marketId),row.names=F,quote=F)
}