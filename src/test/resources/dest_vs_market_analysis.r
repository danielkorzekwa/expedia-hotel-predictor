sqldf('select srch_destination_id,count(*) from test group by srch_destination_id order by count(*) desc limit 10')

sqldf('select hotel_cluster,count(*) from test where srch_destination_id= 8279 group by hotel_cluster order by count(*) desc limit 10')

sqldf('select hotel_market,count(*) from train where srch_destination_id= 8279 group by hotel_market order by count(*) desc limit 10')

sqldf('select hotel_cluster,count(*) from train where srch_destination_id= 8279 and hotel_market=190 group by hotel_cluster order by count(*) desc limit 10')