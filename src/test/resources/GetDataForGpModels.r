n <- 14
train_countries <- train_13b[hotel_continent==3,.N,by=hotel_country][order(+N)][1:n]
test_countries <- test_all[hotel_continent==3,.N,by=hotel_country][order(+N)][1:n]

train_gp <- train_13b[hotel_country %in% train_countries$hotel_country]
test_gp <- test_all[hotel_country %in% test_countries$hotel_country]





write.csv(train_gp,'c:/perforce/daniel/ex/segments/cont_3_30smallest_countries/train_2013_booked_only.csv',row.names=F,quote=F)
write.csv(test_gp,'c:/perforce/daniel/ex/segments/cont_3_30smallest_countries/train_2014_booked_only.csv',row.names=F,quote=F)