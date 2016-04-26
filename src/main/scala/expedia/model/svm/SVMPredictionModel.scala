package expedia.model.svm

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import expedia.model.svm.libsvm.LibSvmModel
import expedia.model.svm.libsvm.libSvmTrain
import breeze.linalg._
import scala.collection.mutable.ListBuffer
import expedia.model.svm.libsvm.libSvmPredict

/**
 * @param destMatrix [destId,d1,d2,...d149]
 * @trainData [dest,cluster]
 */
case class SVMPredictionModel(destMatrix: DenseMatrix[Double], d149SvmModel: LibSvmModel) {

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
        val probs = libSvmPredict(d149Vec.toDenseMatrix, d149SvmModel)
        Some(probs(0, ::).t)
      }
      case None => None
    }

  }
}