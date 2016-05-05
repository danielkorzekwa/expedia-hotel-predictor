
sqldf('select user_id,count(*) as c from d1 group by user_id having c>40 order by c')
sqldf('select hotel_cluster,count(*) as c from d1 where srch_destination_id=8739  group by hotel_cluster order by c')
sqldf('select srch_destination_id,count(*) as c from d1 where user_id=1137579 group by srch_destination_id order by c')


d1_ab <-subset(d1,hotel_cluster>20 & hotel_cluster < 30)


d1_ab <-subset(test,hotel_market==675)
a<- d1_ab
a$monthOfYear <- format(as.Date(a$srch_ci), "%Y-%m-1")
b <- sqldf('select monthOfYear,count(*) c from a where hotel_cluster = 55 group by monthOfYear')
ggplot() + geom_line(aes(x=as.Date(b$monthOfYear),y=b$c)) +coord_cartesian(ylim=c(0,1000))