sqldf('select srch_destination_id,count(*) from train_13 group by srch_destination_id order by count(*) desc limit 10')


sqldf('select hotel_market,count(*) from train_13 where srch_destination_id=8250 group by hotel_market order by count(*) desc limit 10')


