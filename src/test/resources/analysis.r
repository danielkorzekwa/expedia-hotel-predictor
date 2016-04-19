rm(list=ls())

#
# Read data
#

d1 <- read.csv('train_booked_all.csv',nrow=50000000)
d1_a <- subset(d1,as.Date(date_time)<as.Date('2014-01-01'))[1:10000,]
d1_b <- subset(d1,as.Date(date_time)>=as.Date('2014-01-01'))[1:10000,]

d1_a <- subset(d1_a,srch_destination_id==8250)
d1_b <- subset(d1_b,srch_destination_id==8250)

dest <- read.csv('destinations.csv')
d1_a <- merge(d1_a,dest)
d1_b <- merge(d1_b,dest)

rownames(d1_a) <- seq(length=nrow(d1_a))
rownames(d1_b) <- seq(length=nrow(d1_b))

category <- c('is_package','srch_destination_id','hotel_continent','hotel_market','hotel_cluster')
write.csv(d1_a[category],'train_booked_sample_a.csv',row.names=FALSE)
write.csv(d1_b[category],'train_booked_sample_b.csv',row.names=FALSE)

#
#Analysis
#
cluster<-90
ggplot() + geom_density(aes(col='all',x=subset(d1_ad,hotel_cluster!=cluster)$d29))  +
 geom_density(aes(col='single',x=subset(d1_ad,hotel_cluster==cluster)$d29)) + coord_cartesian(xlim=c(-2.5,-1))

qplot(subset(d1_ad,hotel_cluster!=90)$d1) + coord_cartesian(xlim=c(-2.50,-1.5))
ggplot() +stat_summary(fun.y=mean,geom='point',aes(x=d$orig_destination_distance,y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))
ggplot() +stat_summary(fun.y=mean,geom='line',aes(x=as.Date(d$date_time),y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))


ggplot() +geom_smooth(aes(x=as.Date(d$srch_ci),y=d$is_booking)) + coord_cartesian(ylim=c(-0.1,1.1))

sqldf('select srch_destination_id,count(*) as c from d1_b where  hotel_cluster=90 group by srch_destination_id order by c')
sqldf('select hotel_cluster,count(*) as c from d1_a where srch_destination_id>-33520 group by hotel_cluster order by c')

#Read predictions
p <- read.csv('predictions.csv')

#SVM

svmData_a <- subset(d1_a[1:10000,24:173],hotel_cluster>-1)
svmData_b <- subset(d1_a[1:10000,24:173],hotel_cluster>-1)
svmData_a$hotel_cluster <- as.factor(svmData_a$hotel_cluster)
svmData_b$hotel_cluster <- as.factor(svmData_b$hotel_cluster)
sqldf('select hotel_cluster,count(*) as c from svmData_a group by hotel_cluster order by c')

model <- svm(hotel_cluster ~ ., data = svmData_a)
p$svm <- predict(model,svmData_b)
qplot(p$svm)

model <- naiveBayes(as.factor(hotel_cluster) ~ d9+d16+d29, data = d1_ad)
d1_ad$naive <- predict(model,d1_ad)
qplot(d1_ad$naive)