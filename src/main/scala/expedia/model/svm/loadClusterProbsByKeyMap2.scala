package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._

object loadClusterProbsByKeyMap2 {
  
  /**
   * @param svmPredictionsData [c0,c1,c99,srch_dest_id]
   */
  def apply[T](svmPredictionsData:DenseMatrix[Double]):Map[T,DenseVector[Float]] = {
     val clusterProbsByDestMap: mutable.Map[T, DenseVector[Float]] = mutable.Map()


    (0 until svmPredictionsData.rows).foreach { i =>
      val row = svmPredictionsData(i, ::).t
      val key = row(100).asInstanceOf[T]
      val clusterProbsVector = row(0 to 99).map(x => x.toFloat)

    clusterProbsByDestMap.update(key, clusterProbsVector)
    }

    clusterProbsByDestMap.toMap
  }
}