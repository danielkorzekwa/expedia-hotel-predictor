rm(list=ls())

train_13 <- fread('c:/perforce/daniel/ex/data_all/train_all_2013.csv')



source('calcClusterProbsSVM.r')
train <- subset(train_13,srch_destination_id==12217 & is_booking==1 & (hotel_cluster==23 | hotel_cluster==21))
train <- subset(train,as.Date(srch_ci)<as.Date('2014-01-01'))
train$month <- month(as.Date(train$srch_ci))
train$length <- as.Date(train$srch_co) - as.Date(train$srch_ci)
train <- train[,c('month','hotel_cluster'),with=F]

predicted <- calcClusterProbsSVM(train,train)

ggplot() + geom_line(aes(col='4',x=train$month,y=predicted$X4))  +
 geom_line(aes(col='49',x=train$month,y=predicted$X21)) +
  coord_cartesian(ylim=c(-0.1,1.1))
 
train$hotel_cluster <- ifelse(train$hotel_cluster==19,1,0)
ggplot() +  stat_summary(fun.y = "mean", geom = "point",aes(x=train$month,y=train$hotel_cluster)) 

model <- svm(hotel_cluster ~ ., data = train,scale=F)
print(summary(model))

svmProbs <- predict(model,train)
