package expedia.model.distgp

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._

object createClusterProbsByDistMap {

  /**
   * key distance, value - cluster probs vector
   */
  def apply(clusterProbsByDistData: DenseMatrix[Double]): Map[Double, DenseVector[Double]] = {
    val clusterProbsByDisMap: mutable.Map[Double, DenseVector[Double]] = mutable.Map()

    (0 until clusterProbsByDistData.rows).foreach { i =>
      val row = clusterProbsByDistData(i, ::).t
      val dist = row(0)
      val clusterProbs = row(1 until row.size)

      clusterProbsByDisMap.update(dist, clusterProbs)
    }

    clusterProbsByDisMap.toMap
  }
}