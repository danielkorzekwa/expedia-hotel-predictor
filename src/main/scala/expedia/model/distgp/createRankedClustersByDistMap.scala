package expedia.model.distgp

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._

object createRankedClustersByDistMap {

  /**
   * key distance, value - ranked cluster
   */
  def apply(rankedClustersByDistData: DenseMatrix[Double]): Map[Double, DenseVector[Int]] = {
    val rankedClustersByDisMap: mutable.Map[Double, DenseVector[Int]] = mutable.Map()

    (0 until rankedClustersByDistData.rows).foreach { i =>
      val row = rankedClustersByDistData(i, ::).t
      val dist = row(0)
      val rankedClusters = row(1 until row.size).map(x => x.toInt)

      rankedClustersByDisMap.update(dist, rankedClusters)
    }

    rankedClustersByDisMap.toMap
  }
}