package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import breeze.linalg._
import scala.collection.mutable.ListBuffer
import expedia.model.svm.libsvm.svc.svcPredict
import expedia.model.svm.libsvm.svc.SvcModel

/**
 * @param destMatrix [destId,d1,d2,...d149]
 * @trainData [dest,cluster]
 */
case class SVMPredictionModel(destMatrix: DenseMatrix[Double], d149SvmModel: SvcModel) {

  private val d149ByDestMap = calcD149ByDestMap(destMatrix)

  /**
   * @param row [destId]
   *
   * @param [cluster0 prob, cluster 1 prob,...,cluster99 prob]
   */
  def predict(destId: Double): Option[DenseVector[Double]] = {

    val d149Vec = d149ByDestMap.get(destId)
    d149Vec match {
      case Some(d149Vec) => {
        val probs = svcPredict(d149Vec.toDenseMatrix, d149SvmModel)
        Some(probs(0, ::).t)
      }
      case None => None
    }

  }
}