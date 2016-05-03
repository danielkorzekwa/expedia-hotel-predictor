package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._

object calcSimilarClustersMap {

  /**
   * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
   * @return Matrix of sorted cluster similarities, row0=[0,4,5,87...], row1=[1,65,34,12...]....row99
   */
  def apply(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]): DenseMatrix[Double] = {

    val clusterSimMatrix = DenseMatrix.fill(100, 100)(0d)
    val similarClustersMap: mutable.Map[Double, mutable.HashSet[Double]] = mutable.Map()

    clusterByDistMap.foreach {
      case (key, clusters) =>

        clusters.foreach { cluster =>
          clusters.foreach { otherCluster => clusterSimMatrix(cluster.toInt, otherCluster.toInt) += 1d }
        }

    }

    val sortedClusterSimMatrix = clusterSimMatrix(*, ::) map { row =>
      DenseVector(row.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray)
    }
    
    sortedClusterSimMatrix
  }
}