package expedia.model.clusterdistbayes

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._

/**
 * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
 */
case class ClusterDistBayesPredictionModel(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]) extends LazyLogging {

  logger.info("DistClusterMap size=%d".format(clusterByDistMap.size))

  
  def predict(userLoc: Double, dist: Double, market: Double, hotelCluster: Double): Double = {

    
    val key = (userLoc, dist, market)
    val clusterVec = clusterByDistMap.get(key)
    val prob2 = clusterVec match {
      case Some(clusterVec) => clusterVec(hotelCluster.toInt)
      case None => Double.NaN
    }
    
   prob2

  }

}