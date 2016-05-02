package expedia.model.clusterdistbayes

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._

object calcClusterCPD {

  /**
   * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
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

    val clusterCPD = clusterSimMatrix(*, ::) map { row =>
      
      val Z = sum(row)
      
      row/Z
    }
    
  clusterCPD
  }
}