package expedia.model.userdestsim

import com.typesafe.scalalogging.slf4j.LazyLogging

import expedia.model.clusterdist.calcClusterCoExistMatrix
import expedia.model.clusterdist.calcSimilarClustersMap

/**
 * @param clusterByDistMap Map[(userId, destId),[all clusters for the key]]
 */
case class UserDestSimPredictionModel(clustersByUserDestMap: Map[Tuple2[Double, Double], List[Double]]) extends LazyLogging {

  logger.info("UserDestSimClusterMap size=%d".format(clustersByUserDestMap.size))

  val topClusterByUserDestMap = clustersByUserDestMap.map {
    case (key, clusters) =>

      val sortedClusters = clusters.groupBy { c => c }.map { case (key, keyClusters) => key -> keyClusters.size }.toList.sortWith((a, b) => a._2 > b._2).map(_._1)
      (key, sortedClusters)
  }

  val distClutersSeq = topClusterByUserDestMap.map { case (key, clusters) => clusters }.toList
  val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)
  val similarClustersMatrix = calcSimilarClustersMap(clusterCoExistMat)

  def predict(userId: Double, destId: Double, hotelCluster: Double): Double = {

    val key = (userId, destId)
    val clusterVec = topClusterByUserDestMap.get(key)
    val prob2 = clusterVec match {
      case Some(clusterVec) => {
        val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
        val prob = if (clusterIndex == -1d) {

          if (clusterVec.size > 0 && clusterVec.size <= 2) {
            val topCluster = clusterVec(0)

            if (hotelCluster == similarClustersMatrix(topCluster.toInt, 1)) {
              0.99
            } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 2)) {
              0.99 - 0.0001
            } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 3)) {
              0.99 - 0.0002
            } else if (hotelCluster == similarClustersMatrix(topCluster.toInt, 4)) {
              0.99 - 0.0003
            } else Double.NaN
          } else Double.NaN
        } else 1d - 0.0001 * clusterIndex
        prob
      }
      case None => Double.NaN
    }

    prob2

  }
}