library(sqldf)
library(data.table)

rm(list=ls())

train_13 <- fread('c:/perforce/daniel/ex/data_all/train_all_2013.csv')
train_13_booked <- subset(train_13,is_booking==1)
test_all <- fread('c:/perforce/daniel/ex/data_booked/train_booked_2014_all_cols.csv')

destIds <- data.frame(dest_id=c(8824))
#destIds <- sqldf('select srch_destination_id,count(*) as c from train_13_booked group by srch_destination_id having count(*)>1600 and count(*)<2000 order by c desc limit 10' )
#destIds <- destIds['srch_destination_id']
write.csv(destIds,'c:/perforce/daniel/ex/segments/destmonthdata/destIds.csv',row.names=F)

#Save training set
for(i in 1:nrow(destIds)) {
  destId <- destIds[i,1]
  print(sprintf('Saving training data for destId = %d',destId))
  s <- subset(train_13,  srch_destination_id==destId & is_booking==1)

  write.csv(s,sprintf('c:/perforce/daniel/ex/segments/destmonthdata/train_2013_dest%d_booked_only.csv',destId),row.names=F)
}

#Save train single file
trainSet <-train_13[0,]
for(i in 1:nrow(destIds)) {
  destId <- destIds[i,1]
  s <- subset(train_13,  srch_destination_id==destId)
  trainSet <- rbind(trainSet,s)
}
write.csv(trainSet,sprintf('c:/perforce/daniel/ex/segments/destmonthdata/train_2013.csv',destId),row.names=F)

#Save test single file
testSet <-test_all[0,]
for(i in 1:nrow(destIds)) {
  destId <- destIds[i,1]
  s <- subset(test_all,  srch_destination_id==destId & is_booking==1)
  testSet <- rbind(testSet,s)
}
write.csv(testSet,sprintf('c:/perforce/daniel/ex/segments/destmonthdata/train_2014_booked_only.csv',destId),row.names=F)