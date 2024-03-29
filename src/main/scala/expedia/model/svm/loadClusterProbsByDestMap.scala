package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._

object loadClusterProbsByKeyMap {
  
  /**
   * @param svmPredictionsData [c0,c1,c99,key]
   */
  def apply(svmPredictionsData:DenseMatrix[Double]):Map[Int,DenseVector[Float]] = {
     val clusterProbsByKeyMap: mutable.Map[Int, DenseVector[Float]] = mutable.Map()

    (0 until svmPredictionsData.rows).foreach { i =>
      val row = svmPredictionsData(i, ::).t
      val key = row(100).toInt
      val clusterProbsVector = row(0 to 99).map(x => x.toFloat)

    clusterProbsByKeyMap.update(key, clusterProbsVector)
    }

    clusterProbsByKeyMap.toMap
  }
}