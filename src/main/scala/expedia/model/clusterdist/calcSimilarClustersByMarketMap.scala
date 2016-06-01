package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import scala.collection._

object calcSimilarClustersByMarketMap {

  /**
   * @param m Matrix of cluster co-existences
   * @return Matrix of sorted cluster similarities, row0=[0,4,5,87...], row1=[1,65,34,12...]....row99
   */
  def apply(clusterCoExistMatrixByMarketId: Map[Int, DenseMatrix[Double]]): Map[Int, DenseMatrix[Double]] = {

    val sortedClusterSimMatrixByMarket = clusterCoExistMatrixByMarketId.map {
      case (key, coexistMat) =>
        val sortedClusterSimMatrix = coexistMat(*, ::) map { row =>
          DenseVector(row.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray)
        }
        key -> sortedClusterSimMatrix
    }

    sortedClusterSimMatrixByMarket
  }
}