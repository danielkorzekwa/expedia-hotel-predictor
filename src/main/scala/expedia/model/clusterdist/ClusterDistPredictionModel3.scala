package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._

/**
 * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
 */
case class ClusterDistPredictionModel3(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]) extends LazyLogging {

  logger.info("DistClusterMap size=%d".format(clusterByDistMap.size))

   private val similarClustersMatrix = calcSimilarClustersMap(clusterByDistMap)
  
  def predict(userLoc: Double, dist: Double, market: Double, hotelCluster: Double): Double = {

    
    val key = (userLoc, dist, market)
    val clusterVec = clusterByDistMap.get(key)
    val prob2 = clusterVec match {
      case Some(clusterVec) => {
        val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
        val prob = if (clusterIndex == -1d) {
          
          if( clusterVec.size<=2) {
            if (hotelCluster==similarClustersMatrix(clusterVec(0).toInt,1)) {
              0.99
              } 
            else if (hotelCluster==similarClustersMatrix(clusterVec(0).toInt,2)) {
              0.99-0.0001
              }
              else if (hotelCluster==similarClustersMatrix(clusterVec(0).toInt,3)) {
              0.99-0.0002
              }
              else if (hotelCluster==similarClustersMatrix(clusterVec(0).toInt,4)) {
              0.99-0.0003
              }
            
            else Double.NaN
          }
          else Double.NaN
        }
        else 1d - 0.0001 * clusterIndex
        prob
      }
      case None => Double.NaN
    }
    
   prob2

  }

}