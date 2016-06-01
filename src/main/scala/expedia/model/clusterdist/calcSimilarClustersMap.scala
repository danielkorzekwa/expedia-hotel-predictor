package expedia.model.clusterdist

import scala.collection._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._

object calcSimilarClustersMap {

  /**
   * @param m Matrix of cluster co-existences
   * @return Matrix of sorted cluster similarities, row0=[0,4,5,87...], row1=[1,65,34,12...]....row99
   */
  def apply(coExistClusterMatrix: DenseMatrix[Double]): DenseMatrix[Double] = {
    
    val sortedClusterSimMatrix = coExistClusterMatrix(*, ::) map { row =>
      DenseVector(row.toArray.toList.zipWithIndex.sortWith((a, b) => a._1 > b._1).map(_._2.toDouble).toArray)
    }
    
    sortedClusterSimMatrix
  }
}