package expedia.model.clusterdist

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scala.collection._

object calcClusterCoExistMatrix {

  /**
   * @param distClutersSeq Seq[clusters] for n cluster distance keys
   * @return Matrix of cluster co-existences
   */
  def apply(distClutersSeq: Seq[DenseVector[Int]]): DenseMatrix[Double] = {

    val clusterSimMatrix = DenseMatrix.fill(100, 100)(0d)

    distClutersSeq.foreach {
      clusters =>
        clusters.foreach { cluster =>
          clusters.foreach { otherCluster =>
            clusterSimMatrix(cluster.toInt, otherCluster.toInt) += 1d
          }
        }

    }

    clusterSimMatrix
  }
}