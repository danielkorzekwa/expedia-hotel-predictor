package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._

case class ClusterDistPredictionModel(expediaTrainFile: String) extends LazyLogging {

  logger.info("Loading distClusterMap...")
//  private val clustMap = calcClusterByDistMap(expediaTrainFile)

  //Map[(user_location_city,orig_destination_distance,hotel_market),DenseVector[sortedClustersByCount]]
  private val clustMap2: Map[Tuple3[Double, Double, Double], DenseVector[Double]] = calcClusterByDistMap2(expediaTrainFile)

  logger.info("Loading distClusterMap...done: clusterMapSize=%d".format(clustMap2.size))

  /**
   * @param data [user_location_city,orig_destination_distance,hotel_market]
   */
  def predict(row: DenseVector[Double], hotelCluster: Double): Double = {

//    val key = (row(0), row(1), row(2), hotelCluster)
//    val prob = if (clustMap.contains(key)) 1d else Double.NaN

    
    val key2 = (row(0), row(1), row(2))
    val clusterVec = clustMap2.get(key2)
    val prob2 = clusterVec match {
      case Some(clusterVec) => {
        val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
        val prob = if(clusterIndex == -1d) Double.NaN
        else  1d - 0.0001*clusterIndex
        prob
      }
      case None => Double.NaN
    }
    prob2

  }
}