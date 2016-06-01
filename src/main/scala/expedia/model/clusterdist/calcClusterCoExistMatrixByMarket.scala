package expedia.model.clusterdist

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._

object calcClusterCoExistMatrixByMarket {

  /**
   * @param  topClusterByDistMap Map[(userLoc,dist,market,dest),[all clusters for the key]]
   * @return Matrix of cluster co-existences
   */
  def apply(topClusterByDistMap: Map[Tuple4[Int, Int, Int, Int], DenseVector[Int]]): Map[Int, DenseMatrix[Double]] = {

    val clusterCoExistMatrixByMarketId: mutable.Map[Int, DenseMatrix[Double]] = mutable.Map()

    topClusterByDistMap.foreach {
      case (key, topClusters) =>
        val marketId = key._3
        val coexistMatrix = clusterCoExistMatrixByMarketId.getOrElseUpdate(marketId, DenseMatrix.fill(100, 100)(0d))

        topClusters.foreach { cluster =>
          topClusters.foreach { otherCluster =>
            coexistMatrix(cluster.toInt, otherCluster.toInt) += 1d
          }
        }
    }

    clusterCoExistMatrixByMarketId
  }
}