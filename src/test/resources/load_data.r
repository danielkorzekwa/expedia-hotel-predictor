# Test data


t<- read.csv('orig_data/test.csv')
t <- t[-c(1)]
t$is_booking <- ''
t$cnt <- ''
t <- t[ , c(1:18,22,23,19,20,21)]


write.csv(t,'test_all.csv',row.names=FALSE,quote=FALSE)
