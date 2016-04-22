sqldf('select user_id,count(*) c from d1_a group by user_id order by c desc limit 10')

sqldf('select hotel_cluster,count(*) c from d1_b where user_id=331829 group by hotel_cluster order by c desc limit 10')

#no match
331829

#strong match
195876