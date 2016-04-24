sqldf('select user_id,count(*) c from d1_a group by user_id order by c desc limit 10')

sqldf('select hotel_cluster,count(*) c from d1_a where user_id=1156294 group by hotel_cluster order by c desc limit 10')

sqldf('select hotel_cluster,count(*) c from d1_a where srch_adults_cnt>3 and srch_children_cnt>1 group by hotel_cluster order by c desc limit 10')

sqldf('select hotel_cluster,count(*) c from d1_a where srch_adults_cnt>3 and srch_rm_cnt>1 and srch_destination_id=8253 group by hotel_cluster order by c desc limit 100')


#no match
331829

#strong match
195876