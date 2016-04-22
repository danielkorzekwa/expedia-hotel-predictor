# @param d user probs data frame: [user_id,item0,item1,item2,...,item3]
# @return [user_id,user_segment]
calcUserSegments <- function(d) {

  s <- svd(d[,-c(1)])
  
  # raw data kmeans
 # user_segment <- kmeans(d[,-c(1)],centers=2)$cluster
  
  #svd k means
  user_segment <- kmeans(s$u,centers=2)$cluster
  
  user_id <- d$user_id
  return(data.frame(user_id,user_segment))
}

