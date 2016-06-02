library(sqldf)
library(data.table)

rm(list=ls())

train_13 <- fread('c:/perforce/daniel/ex/orig_data/train.csv')
train_13 <- subset(train_13,!is.na(orig_destination_distance))

test_all <- fread('c:/perforce/daniel/ex/data_test/test_all_all_cols.csv')

userLocMarketList <- sqldf('select user_location_city,hotel_market,srch_destination_id, count(*) c from train_13 group by user_location_city,hotel_market,srch_destination_id having count(*)>100 order by count(*) desc')

write.csv(userLocMarketList,'c:/perforce/daniel/ex/segments/loc_market_dest_submission/userLocMarketList.csv',row.names=F)

#Save training set
for(i in 1:nrow(userLocMarketList)) {
  userLoc <- userLocMarketList[i,1]
  marketId <- userLocMarketList[i,2]
  destId <- userLocMarketList[i,3]
  print(sprintf('Saving training data %d',i))
  s <- subset(train_13,   user_location_city==userLoc & hotel_market==marketId & srch_destination_id==destId)
  write.csv(s,sprintf('c:/perforce/daniel/ex/segments/loc_market_dest_submission/train_2013-2014_loc_%d_market_%d_dest_%d.csv',userLoc,marketId,destId),row.names=F,quote=F)
}

#Save test set
for(i in 1:nrow(userLocMarketList)) {
  userLoc <- userLocMarketList[i,1]
  marketId <- userLocMarketList[i,2]
  destId <- userLocMarketList[i,3]
  print(sprintf('Saving test data %d',i))
  s <- subset(test_all,   user_location_city==userLoc & hotel_market==marketId & srch_destination_id==destId)
  write.csv(s,sprintf('c:/perforce/daniel/ex/segments/loc_market_dest_submission/train_2015_booked_only_loc_%d_market_%d_dest_%d.csv',userLoc,marketId,destId),row.names=F,quote=F)
}
