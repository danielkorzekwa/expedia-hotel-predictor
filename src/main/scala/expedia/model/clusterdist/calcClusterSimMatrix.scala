package expedia.model.clusterdist

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._

object calcClusterSimMatrix {
  
  /**
   * @param clusterByDistMap Map[(userLoc,dist,market),[sorted clusters vector by cluster counts]]
   * @return Matrix of cluster co-existences 
   */
  def apply(clusterByDistMap: Map[Tuple3[Double, Double, Double], DenseVector[Double]]): DenseMatrix[Double] = {

    val clusterSimMatrix = DenseMatrix.fill(100, 100)(0d)
    val similarClustersMap: mutable.Map[Double, mutable.HashSet[Double]] = mutable.Map()

    clusterByDistMap.foreach {
      case (key, clusters) =>

        val clustersList = clusters.toArray.toList
        
       clustersList.zipWithIndex.foreach { case (cluster,index) =>
          clustersList.zipWithIndex.foreach { case (otherCluster,otherIndex) => 
            clusterSimMatrix(cluster.toInt, otherCluster.toInt) += 1d 
            }
        }

    }

   clusterSimMatrix
  }
}