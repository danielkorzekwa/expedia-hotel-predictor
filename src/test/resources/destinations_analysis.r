dest <- fread('c:/perforce/daniel/ex/orig_data/destinations.csv')

train_13[hotel_continent==2 & hotel_market==701 & srch_destination_id==8260][,.N,by=hotel_cluster][order(-N)][1:10]

train_13[hotel_continent==2 & hotel_market==628][,.N,by=srch_destination_id][order(+N)][1:10]

clusters <- kmeans(dest[,2:150,with=F],200,iter.max=100)
dest$cluster <- clusters$cluster
destClusters <- dest[,c('srch_destination_id','cluster'),with=F]
write.csv(destClusters,'c:/perforce/daniel/ex/statistics/clusterByDest.csv',row.names=F,quote=F)