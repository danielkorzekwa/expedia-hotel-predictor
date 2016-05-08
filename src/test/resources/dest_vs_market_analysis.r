sqldf('select srch_destination_id,count(*) from train group by srch_destination_id order by count(*) desc limit 10')

sqldf('select hotel_cluster,count(*) from train where srch_destination_id= 8260 group by hotel_cluster order by count(*) desc limit 10')

sqldf('select hotel_market,count(*) from train where srch_destination_id=8250 group by hotel_market order by count(*) desc limit 10')


sqldf('select srch_destination_id,count(*) from train where hotel_market==110 group by srch_destination_id order by count(*) desc limit 10')


sqldf('select hotel_cluster,count(*) from train where srch_destination_id= 8250 and hotel_market=628 group by hotel_cluster order by count(*) desc limit 10')