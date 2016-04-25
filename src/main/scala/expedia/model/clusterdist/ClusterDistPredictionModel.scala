package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import expedia.model.clusterdist.calcClusterByDistMap

case class ClusterDistPredictionModel(expediaTrainFile: String) extends LazyLogging {

  logger.info("Loading distClusterMap...")
  private val clustMap = calcClusterByDistMap(expediaTrainFile)
  logger.info("Loading distClusterMap...done: clusterMapSize=%d".format(clustMap.size))

  /**
   * @param data [user_location_city,orig_destination_distance,hotel_market]
   */
  def predict(row: DenseVector[Double], hotelCluster: Double): Double = {

    val key = (row(0), row(1), row(2))
    val cluster = clustMap.getOrElse(key, -1d)

    if (cluster == hotelCluster) 1d else Double.NaN

  }
}