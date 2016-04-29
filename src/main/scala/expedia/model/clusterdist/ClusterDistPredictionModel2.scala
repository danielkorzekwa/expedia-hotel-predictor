package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._

/**
 * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
 */
case class ClusterDistPredictionModel2(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]) extends LazyLogging {

 
  
  logger.info("DistClusterMap size=%d".format(clusterByDistMap.size))

  def predict(userLoc: Double, dist: Double, market: Double, hotelCluster: Double): Double = {

    val key = (userLoc, dist, market)
    val clusterVec = clusterByDistMap.get(key)
    val prob2 = clusterVec match {
      case Some(clusterVec) => {
        val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
        val prob = if (clusterIndex == -1d) {

          if (hotelCluster == 42) {

           
            val clusterIndex = clusterVec.toArray.toList.indexOf(72)
            if (clusterIndex == -1d) Double.NaN
            else {
              println("aaaa")
              0.99
            }
          } 
          else  if (hotelCluster == 91) {

            val clusterIndex = clusterVec.toArray.toList.indexOf(41)
            val clusterIndex2 = clusterVec.toArray.toList.indexOf(48)
            if (clusterIndex == -1d && clusterIndex2 == -1d) Double.NaN
            else {
              println("bbbbb")
              0.99
            }
          } 
          
          
          else Double.NaN
        } else 1d - 0.0001 * clusterIndex
        prob
      }
      case None => Double.NaN
    }
    prob2

  }

}