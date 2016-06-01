package expedia.model.clusterdist

import breeze.linalg.DenseVector
import com.typesafe.scalalogging.slf4j.LazyLogging
import scala.collection._
import breeze.linalg.DenseMatrix
import expedia.data.Click
import java.util.concurrent.atomic.AtomicInteger
import expedia.model.ClusterModel

/**
 * @param topClusterByDistMap Map[(userLoc,dist,market,dest),[all clusters for the key]]
 */
case class ClusterDistPredictionModel(topClusterByDistMap: Map[Tuple4[Int,Int, Int, Int], DenseVector[Int]]) extends ClusterModel with LazyLogging {

  logger.info("DistClusterMap size=%d".format(topClusterByDistMap.size))

  val distClutersSeq = topClusterByDistMap.map { case (key, clusters) => clusters }.toList

  val clusterCoExistMat = calcClusterCoExistMatrix(distClutersSeq)
  val similarClustersMatrix = calcSimilarClustersMap(clusterCoExistMat)

  
   
 def predict(click:Click): DenseVector[Float] = {
   predict(click.userLoc,click.dist,click.marketId,click.destId)
 }

  def predict(userLoc: Int, dist: Double, market: Int,destId:Int): DenseVector[Float] = {

    val minCounts=1d
    val clusterProbs = DenseVector.tabulate[Float](100) { hotelCluster =>

      val key = (userLoc, (dist * 10000).toInt, market,destId)
      val marketClusterCoExistMat = clusterCoExistMat
      val marketSimilarClustersMat = similarClustersMatrix
      val clusterVec = topClusterByDistMap.get(key)
      val prob2 = clusterVec match {
        case Some(clusterVec) => {
          val clusterIndex = clusterVec.toArray.toList.indexOf(hotelCluster)
          val prob = if (clusterIndex == -1d) {

            if (clusterVec.size > 0 && clusterVec.size <= 4) {
              val topCluster = clusterVec(0)

              if (hotelCluster == marketSimilarClustersMat(topCluster.toInt, 1)) {

                val counts = marketClusterCoExistMat(topCluster,hotelCluster).toInt  
                if(counts>= minCounts) 0.5f - 0.0 else 0f
              } else if (hotelCluster == marketSimilarClustersMat(topCluster.toInt, 2)) {
               val counts = marketClusterCoExistMat(topCluster,hotelCluster).toInt  
                if(counts>= minCounts) 0.5f - 0.01 else 0f
              } else if (hotelCluster == marketSimilarClustersMat(topCluster.toInt, 3)) {
                  val counts = marketClusterCoExistMat(topCluster,hotelCluster).toInt  
                if(counts>= minCounts) 0.5f - 0.02 else 0f
              } else if (hotelCluster == marketSimilarClustersMat(topCluster.toInt, 4)) {
                val counts = marketClusterCoExistMat(topCluster,hotelCluster).toInt  
                if(counts>= minCounts) 0.5f - 0.03 else 0f
              } else 0f
            } else 0f
          } else 1f - 0.01 * clusterIndex
          prob.toFloat
        }
        case None => 0f
      }

      prob2

    }

    clusterProbs
  }
}