package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._
import breeze.linalg.DenseMatrix

/**
 * @param clusterByDistMap Map[(userLoc,dist,market),[all clusters for the key]]
 */
case class ClusterDistPredictionModel3(clustersByDistMap: Map[Tuple3[Double, Double, Double], List[Double]]) extends LazyLogging {

  logger.info("DistClusterMap size=%d".format(clustersByDistMap.size))

  val topClusterByDistMap = clustersByDistMap.map {
    case (key, clusters) =>

      val sortedClusters = clusters.groupBy { c => c }.map { case (key, keyClusters) => key -> keyClusters.size }.toList.sortWith((a, b) => a._2 > b._2).map(_._1)
      (key, sortedClusters)
  }

  val distClutersSeq = topClusterByDistMap.map { case (key, clusters) => clusters }.toList

  val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)

  val similarClustersMatrixByCluster: Map[Double, DenseMatrix[Double]] = (0 until 100).map { cluster =>

    val distClutersSeqForCluster = distClutersSeq.filter { clusters => clusters.contains(cluster) }

    cluster.toDouble -> calcSimilarClustersMap(calcClusterCoExistMatrix(distClutersSeqForCluster))

  }.toMap

  val similarClustersMatrix = calcSimilarClustersMap(clusterCoExistMat)

  def predict(userLoc: Double, dist: Double, market: Double, hotelCluster: Double): Double = {

    val key = (userLoc, dist, market)
    val clusterVec = topClusterByDistMap.get(key)
    val prob2 = clusterVec match {
      case Some(clusterVec) => {
        val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
        val prob = if (clusterIndex == -1d) {

          if (clusterVec.size > 0 && clusterVec.size <= 2) {
            val topCluster = clusterVec(0)

            if (hotelCluster == similarClustersMatrixByCluster(topCluster)(topCluster.toInt, 1)) {
              0.99
            } else if (hotelCluster == similarClustersMatrixByCluster(topCluster)(topCluster.toInt, 2)) {
              0.99 - 0.0001
            } else if (hotelCluster == similarClustersMatrixByCluster(topCluster)(topCluster.toInt, 3)) {
              0.99 - 0.0002
            } else if (hotelCluster == similarClustersMatrixByCluster(topCluster)(topCluster.toInt, 4)) {
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