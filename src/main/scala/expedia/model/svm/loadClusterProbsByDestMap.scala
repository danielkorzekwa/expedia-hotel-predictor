package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.collection._

object loadClusterProbsByDestMap {
  
  /**
   * @param svmPredictionsData [c0,c1,c99,srch_dest_id]
   */
  def apply(svmPredictionsData:DenseMatrix[Double]):Map[Double,DenseVector[Float]] = {
     val clusterProbsByDestMap: mutable.Map[Double, DenseVector[Float]] = mutable.Map()


    (0 until svmPredictionsData.rows).foreach { i =>
      val row = svmPredictionsData(i, ::).t
      val srchDestId = row(100)
      val clusterProbsVector = row(0 to 99).map(x => x.toFloat)

    clusterProbsByDestMap.update(srchDestId, clusterProbsVector)
    }

    clusterProbsByDestMap.toMap
  }
}