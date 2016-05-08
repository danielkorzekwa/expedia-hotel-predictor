ggplot() + geom_histogram(aes(col='all',x=s$length),binwidth=1,alpha=0.5) 


sqldf('select hotel_cluster,count(*) as c from s where length=10 group by hotel_cluster order by c desc limit 10')